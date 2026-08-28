// The `plan` tool: records a plan as a file on disk instead of leaving it in the transcript.
//
// Borrowed from the community @dreki-gg/pi-plan-mode "plan ledger" idea. Plans go under
// ~/.pi/agent/plans/ rather than into the repo: ~/.pi is bind-mounted writable in every mode
// (pi_wrapper.nix), so plan mode can record a plan while still being read-only toward $PWD,
// and plans do not belong in project history. This mirrors Claude Code's own ~/.claude/plans/.
//
// The tool is registered in all modes rather than being added to and removed from the active
// tool set per mode: writing a plan file is never harmful, and keeping it out of the tool-set
// juggling keeps that logic to just `edit`/`write`.
//
// The plan file also carries the todo list (syncTodos, called from index.ts on turn_end). The
// todo tool keeps its state in the transcript, which dies with the session; the copy in the
// plan file is what a fresh session can still read.

import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";
import type { TodoItem } from "./todo.ts";

const MAX_PLANS = 20;

// The task list lives in a delimited block so a sync can rewrite it in place, leaving the
// steps and evidence the model wrote above it untouched.
const TODOS_START = "<!-- todos:start -->";
const TODOS_END = "<!-- todos:end -->";

const PlanParams = Type.Object({
  title: Type.String({ description: "Short title for the plan" }),
  body: Type.String({
    description:
      "The plan as markdown: numbered steps, the path:line evidence behind them, and how to verify.",
  }),
});

interface PlanDetails {
  path?: string;
  title?: string;
  error?: string;
}

function plansDir(): string {
  const agentDir = process.env.PI_CODING_AGENT_DIR ?? join(homedir(), ".pi", "agent");
  return join(agentDir, "plans");
}

function slugify(title: string): string {
  const slug = title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 50);
  return slug || "plan";
}

/** Delete the oldest plans so the directory does not grow without bound. */
function prune(dir: string): void {
  const files = readdirSync(dir)
    .filter((name) => name.endsWith(".md"))
    .sort();
  for (const stale of files.slice(0, Math.max(0, files.length - MAX_PLANS))) {
    rmSync(join(dir, stale), { force: true });
  }
}

function renderTodoBlock(items: TodoItem[]): string {
  const lines = [TODOS_START, "## Task list", ""];
  for (const item of items) {
    lines.push(`- [${item.done ? "x" : " "}] #${item.id} ${item.text}`);
  }
  lines.push(TODOS_END);
  return lines.join("\n");
}

/** Replace the delimited task-list block in `content`, appending it if there is none yet. */
function replaceTodoBlock(content: string, block: string): string {
  const start = content.indexOf(TODOS_START);
  const end = content.indexOf(TODOS_END);
  if (start === -1 || end < start) {
    return `${content.trimEnd()}\n\n${block}\n`;
  }
  return content.slice(0, start) + block + content.slice(end + TODOS_END.length);
}

export interface PlanStore {
  /** Path of the plan written during the current turn, cleared once read. */
  takeWritten: () => string | null;
  /**
   * Mirror the task list into the plan file this session is working against. Returns an error
   * message for the caller to surface, or null on success (including "nothing to do").
   */
  syncTodos: (items: TodoItem[]) => string | null;
}

export function registerPlan(pi: ExtensionAPI): PlanStore {
  let writtenThisTurn: string | null = null;
  let activePlan: string | null = null;

  // Replay the branch to find the plan this session is working against, the same way todo.ts
  // rebuilds its list: the last `plan` result wins, so /resume and /tree pick the right file.
  const reconstruct = (ctx: ExtensionContext): void => {
    activePlan = null;
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type !== "message") continue;
      const message = entry.message;
      if (message.role !== "toolResult" || message.toolName !== "plan") continue;
      const details = message.details as PlanDetails | undefined;
      if (details?.path) {
        activePlan = details.path;
      }
    }
  };

  pi.on("session_start", async (_event, ctx) => reconstruct(ctx));
  pi.on("session_tree", async (_event, ctx) => reconstruct(ctx));

  pi.registerTool({
    name: "plan",
    label: "Plan",
    description:
      "Record an implementation plan as a file. Call this once in plan mode when the plan is " +
      "ready, after investigating. Include the concrete path:line evidence and a verification " +
      "command. Track the individual steps with the `todo` tool as well; they are mirrored into " +
      "this file automatically, so a later session can pick the work up where it stopped.",
    parameters: PlanParams,

    async execute(_toolCallId, params) {
      const dir = plansDir();
      try {
        if (!existsSync(dir)) {
          mkdirSync(dir, { recursive: true });
        }

        const stamp = new Date().toISOString().replace(/[:.]/g, "-");
        const path = join(dir, `${stamp}-${slugify(params.title)}.md`);
        writeFileSync(path, `# ${params.title}\n\n${params.body.trim()}\n`, "utf-8");
        prune(dir);
        writtenThisTurn = path;
        activePlan = path;

        return {
          content: [{ type: "text", text: `Plan written to ${path}` }],
          details: { path, title: params.title },
        };
      } catch (error) {
        // Surface the failure rather than silently losing the plan.
        const message = error instanceof Error ? error.message : String(error);
        return {
          content: [{ type: "text", text: `Failed to write plan: ${message}` }],
          details: { error: message },
          isError: true,
        };
      }
    },

    renderCall(args, theme) {
      return new Text(theme.fg("toolTitle", theme.bold("plan ")) + theme.fg("muted", args.title), 0, 0);
    },

    renderResult(result, _opts, theme) {
      const first = result.content[0];
      const text = first?.type === "text" ? first.text : "";
      return new Text(result.isError ? theme.fg("error", text) : theme.fg("muted", text), 0, 0);
    },
  });

  return {
    takeWritten: () => {
      const path = writtenThisTurn;
      writtenThisTurn = null;
      return path;
    },

    syncTodos: (items) => {
      // An empty list means nothing has been tracked yet; a cleared list keeps whatever was
      // recorded last rather than blanking the plan's task section.
      if (!activePlan || items.length === 0) return null;

      if (!existsSync(activePlan)) {
        const missing = activePlan;
        activePlan = null;
        return `Plan file gone, task list not saved: ${missing}`;
      }

      try {
        const current = readFileSync(activePlan, "utf-8");
        const next = replaceTodoBlock(current, renderTodoBlock(items));
        if (next !== current) {
          writeFileSync(activePlan, next, "utf-8");
        }
        return null;
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return `Failed to save task list to ${activePlan}: ${message}`;
      }
    },
  };
}

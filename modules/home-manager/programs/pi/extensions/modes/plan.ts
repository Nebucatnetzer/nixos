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

import { existsSync, mkdirSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";

const MAX_PLANS = 20;

const PlanParams = Type.Object({
  title: Type.String({ description: "Short title for the plan" }),
  body: Type.String({
    description:
      "The plan as markdown: numbered steps, the path:line evidence behind them, and how to verify.",
  }),
});

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

export interface PlanStore {
  /** Path of the plan written during the current turn, cleared once read. */
  takeWritten: () => string | null;
}

export function registerPlan(pi: ExtensionAPI): PlanStore {
  let writtenThisTurn: string | null = null;

  pi.registerTool({
    name: "plan",
    label: "Plan",
    description:
      "Record an implementation plan as a file. Call this once in plan mode when the plan is " +
      "ready, after investigating. Include the concrete path:line evidence and a verification " +
      "command. Track the individual steps with the `todo` tool as well.",
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
  };
}

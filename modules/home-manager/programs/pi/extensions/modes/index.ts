// Permission modes for pi: plan -> advise, cycled with shift+tab.
//
// Replaces the earlier edit-mode.ts, which had read-only/edit. `advise` is that old read-only
// state unchanged; `plan` mirrors Claude Code's plan mode, meaning investigate, produce a plan,
// then hand off.
//
// There is no write mode. pi_wrapper.nix binds $PWD read-only, so a write would fail anyway.
// This extension keeps `edit`/`write` out of the active tool set and blocks mutating bash, so
// the model gets a clear refusal instead of a filesystem error.
//
// Unlike the upstream plan-mode example this does not save and restore the whole active tool
// set. It only ever removes `edit`/`write`, so tools registered by other extensions
// (web_fetch) and any manual /tools choices survive a mode switch untouched.

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { registerPlan } from "./plan.ts";
import { registerTodo } from "./todo.ts";
import { isDestructive } from "./utils.ts";

type Mode = "plan" | "advise";

const CYCLE: Mode[] = ["plan", "advise"];
const DEFAULT_MODE: Mode = "advise";
const MUTATING_TOOLS = ["edit", "write"];
const STATE_ENTRY = "pi-mode";

const STATUS: Record<Mode, string> = {
  plan: "◔ plan",
  advise: "◎ read-only",
};

const NOTICE: Record<Mode, string> = {
  plan: "Plan mode: investigate and plan, no changes.",
  advise: "Read-only mode: pi will guide changes only.",
};

// Appended to the system prompt each turn for the current mode. The shared "Guiding a
// change" rules live in ai/RULES.md (loaded as AGENTS.md); these only state the mode.
const RULES: Record<Mode, string> = {
  plan: `# Mode: plan

You cannot modify files: \`edit\`/\`write\` are disabled, file-mutating shell commands are
blocked, and the working directory is mounted read-only. Do not try to work around this with
\`bash\`.

Investigate the request and produce a plan. Do **not** emit implementation code in this mode:
no snippets, no diffs. Establish ground truth first and cite it as \`path:line\`.

Record the plan with the \`plan\` tool, then stop. I will choose whether you walk me through it.`,

  advise: `# Mode: advisory (read-only)

You cannot modify files: \`edit\`/\`write\` are disabled, file-mutating shell commands are
blocked, and the working directory is mounted read-only. Do not try to work around this with
\`bash\`.

Guide me through the change following the "Guiding a change" rules: one step at a time, a
targeted snippet with the exact file path, then pause for me to apply it. Use \`bash\` only to
inspect, and to verify after I confirm.`,
};

export default function (pi: ExtensionAPI) {
  let mode: Mode = DEFAULT_MODE;
  let uiContext: ExtensionContext | undefined;

  // The todo list drives the progress widget; refreshWidget is passed in as its change hook.
  const todo = registerTodo(pi, () => refreshWidget());
  const plan = registerPlan(pi);

  pi.registerFlag("plan", {
    description: "Start in plan mode (investigate and plan, no changes)",
    type: "boolean",
    default: false,
  });

  // Every mode is read-only, so the mutating tools are only ever removed. This stays a
  // function because the active tool set is rebuilt on session start and on each mode switch.
  function applyToolState(): void {
    const active = pi.getActiveTools();
    pi.setActiveTools(active.filter((tool) => !MUTATING_TOOLS.includes(tool)));
  }

  function refreshStatus(ctx: ExtensionContext): void {
    uiContext = ctx;
    const items = todo.getItems();
    const progress = items.length ? `  ${items.filter((item) => item.done).length}/${items.length}` : "";
    ctx.ui.setStatus("pi-mode", STATUS[mode] + progress);
    refreshWidget();
  }

  // Progress widget above the editor: the task list, struck through as steps complete. Driven
  // by the todo tool rather than by scraping [DONE:n] markers out of the model's prose.
  function refreshWidget(): void {
    const ui = uiContext?.ui;
    if (!ui) return;

    const items = todo.getItems();
    if (items.length === 0) {
      ui.setWidget("pi-mode-todos", undefined);
      return;
    }

    const theme = ui.theme;
    ui.setWidget(
      "pi-mode-todos",
      items.map((item) =>
        item.done
          ? theme.fg("success", "☑ ") + theme.fg("muted", theme.strikethrough(item.text))
          : theme.fg("muted", "☐ ") + item.text,
      ),
    );
  }

  // Persist as a session entry so the mode survives /resume and branches with /tree.
  function persist(): void {
    pi.appendEntry(STATE_ENTRY, { mode });
  }

  function setMode(next: Mode, ctx: ExtensionContext, notify = true): void {
    mode = next;
    applyToolState();
    refreshStatus(ctx);
    persist();
    if (notify) {
      ctx.ui.notify(NOTICE[mode], "info");
    }
  }

  function cycle(ctx: ExtensionContext): void {
    const next = CYCLE[(CYCLE.indexOf(mode) + 1) % CYCLE.length];
    setMode(next, ctx);
  }

  pi.on("session_start", async (_event, ctx) => {
    // Restore the persisted mode first, then let an explicit launch flag win.
    const restored = ctx.sessionManager
      .getEntries()
      .filter(
        (entry: { type: string; customType?: string }) =>
          entry.type === "custom" && entry.customType === STATE_ENTRY,
      )
      .pop() as { data?: { mode?: Mode } } | undefined;

    if (restored?.data?.mode && CYCLE.includes(restored.data.mode)) {
      mode = restored.data.mode;
    }

    // Only a set flag overrides; it defaults to false, so false never means "force advise".
    if (pi.getFlag("plan") === true) {
      mode = "plan";
    }

    applyToolState();
    refreshStatus(ctx);
  });

  // Close the bash side-channel: with edit/write gone a model can still try to mutate files
  // through the shell (sed -i, redirects, tee, git apply, ...). The read-only bind rejects
  // those anyway, but blocking them gives a clear reason instead of an EROFS error. Read-only
  // shell commands (git diff/log/status, builds, linters, tests) pass through untouched.
  pi.on("tool_call", async (event) => {
    if (event.toolName !== "bash") return undefined;

    const command = (event.input?.command as string) ?? "";
    if (!isDestructive(command)) return undefined;

    return {
      block: true,
      reason:
        `${mode === "plan" ? "Plan" : "Read-only"} mode: file mutation via bash is disabled. ` +
        "Guide the change instead and I will apply it.",
    };
  });

  // Inject the mode's rules per turn rather than as a persistent session message. The upstream
  // plan-mode example injects a message and then needs a `context` handler to filter stale
  // ones back out once the mode changes; chaining systemPrompt avoids that entirely.
  pi.on("before_agent_start", async (event) => ({
    systemPrompt: `${event.systemPrompt}\n\n${RULES[mode]}`,
  }));

  // Keep the status progress counter live as the model toggles steps, and mirror the list into
  // the plan file: the todo tool's own state lives in the transcript, which dies with the
  // session. One coalesced write per turn rather than one per toggle.
  pi.on("turn_end", async (_event, ctx) => {
    refreshStatus(ctx);
    const error = plan.syncTodos(todo.getItems());
    if (error && ctx.hasUI) {
      ctx.ui.notify(error, "warning");
    }
  });

  // The handoff out of plan mode, pi's equivalent of Claude Code's ExitPlanMode. Keyed on a
  // plan file actually having been written this turn, so a clarifying question mid-plan does
  // not trigger the prompt. There is no "apply it yourself" branch: pi cannot apply anything.
  pi.on("agent_end", async (_event, ctx) => {
    const planPath = plan.takeWritten();
    if (mode !== "plan" || !planPath || !ctx.hasUI) return;

    const choice = await ctx.ui.select(`Plan recorded at ${planPath}\n\nWhat next?`, [
      "Walk me through it",
      "Stay in plan mode",
    ]);

    if (choice !== "Walk me through it") return;

    setMode("advise", ctx);

    pi.sendMessage(
      {
        customType: "pi-mode-handoff",
        content:
          "Walk me through the plan one step at a time. Give me the first step now: a targeted snippet with the exact file path, then stop and wait for me to apply it.",
        display: true,
      },
      { triggerTurn: true, deliverAs: "followUp" },
    );
  });

  pi.registerCommand("mode", {
    description: "Show or set the permission mode (plan | advise)",
    handler: async (args, ctx) => {
      const requested = args.trim() as Mode;
      if (!requested) {
        ctx.ui.notify(`Mode: ${mode}. Use /mode ${CYCLE.join("|")} or shift+tab to cycle.`, "info");
        return;
      }
      if (!CYCLE.includes(requested)) {
        ctx.ui.notify(`Unknown mode "${requested}". Expected one of: ${CYCLE.join(", ")}.`, "error");
        return;
      }
      setMode(requested, ctx);
    },
  });

  pi.registerCommand("plan", {
    description: "Enter plan mode (investigate and plan, no changes)",
    handler: async (_args, ctx) => setMode("plan", ctx),
  });

  // shift+tab matches Claude Code. keybindings.json moves pi's app.thinking.cycle off it.
  pi.registerShortcut("shift+tab", {
    description: "Cycle permission mode (plan / read-only)",
    handler: async (ctx) => cycle(ctx),
  });
}

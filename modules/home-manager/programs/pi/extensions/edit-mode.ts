// Read-only-by-default guard with an in-app edit toggle for pi.
//
// pi keeps all built-in tools active by default. This extension deactivates the
// mutating tools (`edit`, `write`) at session start so pi guides changes instead
// of applying them, and lets the user re-enable them at runtime (ctrl+e or /edit),
// mirroring Claude Code's "press a key to allow edits" flow.
//
// It also closes the bash side-channel: with `edit`/`write` gone a model can still
// mutate files through the shell (sed -i, redirects, tee, git apply, ...). While in
// read-only mode a tool_call hook blocks those commands.
//
// Start directly in edit mode with PI_EDIT_MODE=1 (see the `pi --write` wrapper).

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const MUTATING_TOOLS = ["edit", "write"];

// Shell constructs that write to the filesystem. Matched only while read-only.
const MUTATING_SHELL_PATTERNS: RegExp[] = [
  /\bsed\b[^|;&]*\s-i\b/, // sed in-place edit
  /\bperl\b[^|;&]*\s-i\b/, // perl in-place edit
  /\b(rm|mv|cp|dd|truncate|install|shred|mkfifo|mknod)\b/,
  /\b(touch|mkdir|rmdir|ln|chmod|chown|chgrp)\b/,
  /\btee\b/, // tee writes its target file
  /\bpatch\b/, // applies diffs to files
  /\bgit\s+(apply|add|commit|checkout|switch|restore|reset|rm|mv|stash|clean|push|merge|rebase|cherry-pick|revert|tag|init)\b/,
  // Redirection to a real file. Allows >/dev/null, >/dev/stderr and fd dups like 2>&1.
  />>?\s*(?!&|\/dev\/(?:null|stderr|stdout))\S/,
];

function isMutatingShell(command: string): boolean {
  return MUTATING_SHELL_PATTERNS.some((pattern) => pattern.test(command));
}

export default function (pi: ExtensionAPI) {
  let editMode = process.env.PI_EDIT_MODE === "1";

  function applyToolState() {
    const active = pi.getActiveTools();
    if (editMode) {
      pi.setActiveTools([...new Set([...active, ...MUTATING_TOOLS])]);
    } else {
      pi.setActiveTools(active.filter((tool) => !MUTATING_TOOLS.includes(tool)));
    }
  }

  function refreshStatus(ctx: any) {
    ctx?.ui?.setStatus?.("edit-mode", editMode ? "✎ EDIT" : "◎ read-only");
  }

  function toggle(ctx: any) {
    editMode = !editMode;
    applyToolState();
    refreshStatus(ctx);
    ctx?.ui?.notify?.(
      editMode
        ? "Edit mode ON — pi may modify files."
        : "Read-only mode — pi will guide changes only.",
      editMode ? "warning" : "info",
    );
  }

  pi.on("session_start", async (_event, ctx) => {
    applyToolState();
    refreshStatus(ctx);
  });

  // Block file-mutating shell commands while read-only so the model cannot bypass
  // the disabled edit/write tools through bash. Read-only shell (git diff/log/status,
  // tests, linters, builds) passes through untouched.
  pi.on("tool_call", async (event: any) => {
    if (editMode) return undefined;
    if (event.toolName !== "bash") return undefined;
    const command: string = event.input?.command ?? "";
    if (isMutatingShell(command)) {
      return {
        block: true,
        reason:
          "Read-only mode: file mutation via bash is disabled. Guide the change instead, " +
          "or press F2 (or run /edit) to enable edit mode.",
      };
    }
    return undefined;
  });

  pi.registerCommand("edit", {
    description: "Toggle edit mode (allow pi to modify files)",
    handler: async (_args: string, ctx: any) => {
      toggle(ctx);
    },
  });

  // F2 avoids conflicts with pi's built-in editor shortcuts (ctrl+e = cursorLineEnd).
  // Keyboard shortcut is optional API surface; guard so a version without it still loads.
  if (typeof (pi as any).registerShortcut === "function") {
    (pi as any).registerShortcut("f2", {
      description: "Toggle edit mode",
      handler: async (ctx: any) => {
        toggle(ctx);
      },
    });
  }
}

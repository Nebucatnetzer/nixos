// Pure command classification for the mode guard. Kept free of pi imports so it can be
// unit-tested with `node --experimental-strip-types utils.test.ts`.
//
// This is a DENYLIST on purpose. The upstream plan-mode example pairs its denylist with an
// allowlist (`isSafeCommand`), but that allowlist has no `nix`, `pytest`, `cargo` or `make`
// entries, so it would block the very verification commands the advisory workflow ends with.
// Anything not matched here is allowed through.

// Shell constructs that write to the filesystem. Carried over verbatim from the previous
// edit-mode.ts guard so mode `advise` behaves exactly as read-only mode did before.
const FILE_MUTATION_PATTERNS: RegExp[] = [
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

// System-level damage the old guard missed. These matter more now that $PWD is writable.
// Anchored to command position (start of line, or after ; && || |) rather than \b, because a
// bare \b would block read-only commands that merely mention the word — `grep -rn sudo .`
// must still work.
const COMMAND_START = String.raw`(?:^|[;&|]\s*|\$\(\s*)`;

const SYSTEM_PATTERNS: RegExp[] = [
  new RegExp(`${COMMAND_START}(?:sudo|doas|su)\\b`),
  new RegExp(`${COMMAND_START}(?:kill|pkill|killall)\\b`),
  new RegExp(`${COMMAND_START}(?:reboot|shutdown|halt|poweroff)\\b`),
  new RegExp(`${COMMAND_START}systemctl\\s+(?:start|stop|restart|reload|enable|disable|mask)\\b`),
  new RegExp(`${COMMAND_START}service\\s+\\S+\\s+(?:start|stop|restart|reload)\\b`),
  // Interactive editors would hang the tool call waiting on a TTY.
  new RegExp(`${COMMAND_START}(?:vim?|nano|emacs|kak|helix|hx|code|subl)\\b`),
  // Package managers mutate state outside the repo. `install` is already covered above,
  // so these catch the remaining mutating verbs.
  new RegExp(`${COMMAND_START}(?:npm|yarn|pnpm|bun)\\s+(?:add|remove|uninstall|update|upgrade|ci|link|publish)\\b`),
  new RegExp(`${COMMAND_START}pip3?\\s+(?:uninstall|upgrade)\\b`),
  new RegExp(`${COMMAND_START}(?:apt|apt-get|dnf|pacman|brew)\\s+\\S`),
  new RegExp(`${COMMAND_START}(?:nix-env|nixos-rebuild)\\s+(?!.*\\b(?:dry-build|dry-activate|build)\\b)\\S`),
];

export const DESTRUCTIVE_PATTERNS: RegExp[] = [...FILE_MUTATION_PATTERNS, ...SYSTEM_PATTERNS];

/** True when `command` would mutate files or system state. */
export function isDestructive(command: string): boolean {
  return DESTRUCTIVE_PATTERNS.some((pattern) => pattern.test(command));
}

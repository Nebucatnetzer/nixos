// Inject CLAUDE.md or .claude/CLAUDE.md into the agent context if present.
//
// This extension checks for agent context files used by Claude Code:
// - CLAUDE.md (project root)
// - .claude/CLAUDE.md (project root)
//
// When found, the content is injected as a persistent system message so pi
// can see the same agent instructions that Claude Code would use.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { readFileSync, existsSync } from "node:fs";
import { join } from "node:path";

function findClaudeMd(cwd: string): string | null {
  // Check for CLAUDE.md in project root
  const rootPath = join(cwd, "CLAUDE.md");
  if (existsSync(rootPath)) {
    return rootPath;
  }

  // Check for .claude/CLAUDE.md
  const claudeDirPath = join(cwd, ".claude", "CLAUDE.md");
  if (existsSync(claudeDirPath)) {
    return claudeDirPath;
  }

  return null;
}

export default function (pi: ExtensionAPI) {
  let cachedContent: string | null = null;
  let lastCwd: string | null = null;

  pi.on("before_agent_start", async (event, ctx) => {
    // Only inject once per session/cwd to avoid duplicate messages
    if (ctx.cwd !== lastCwd) {
      cachedContent = null;
      lastCwd = ctx.cwd;
    }

    if (cachedContent !== null) {
      // Already injected for this session
      return;
    }

    const claudeMdPath = findClaudeMd(ctx.cwd);
    if (!claudeMdPath) {
      // No CLAUDE.md found, mark as checked
      cachedContent = "";
      return;
    }

    try {
      const content = readFileSync(claudeMdPath, "utf-8").trim();
      if (content.length === 0) {
        cachedContent = "";
        return;
      }

      cachedContent = content;

      // Inject as a system message that persists in the session
      return {
        message: {
          customType: "claude-context",
          content: `# CLAUDE.md (Agent Context)\n\n${content}`,
          display: false,
        },
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`[claude-context] Failed to read ${claudeMdPath}: ${message}`);
      cachedContent = "";
    }
  });
}

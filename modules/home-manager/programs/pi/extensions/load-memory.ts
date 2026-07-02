// Load global memory (MEMORY.md) at session start and inject it into the system prompt.
//
// Pi's home-manager module copies MEMORY.md to ~/.pi/agent/ but does not automatically
// read it into context. This extension ensures the memory content is available to the
// LLM at the start of each session by prepending it to the system prompt.
//
// The memory is injected once per session_start and persists across turns until the
// session ends. Use this for long-term knowledge, project preferences, and lessons
// learned that should guide the agent's behavior.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

const MEMORY_PATH = join(process.env.HOME ?? "~", ".pi/agent/MEMORY.md");

export default function (pi: ExtensionAPI) {
  let memoryContent: string | null = null;

  // Load memory file at session start
  pi.on("session_start", async (_event, ctx) => {
    memoryContent = null;

    if (existsSync(MEMORY_PATH)) {
      try {
        const content = readFileSync(MEMORY_PATH, "utf8").trim();
        if (content.length > 0) {
          memoryContent = content;
          ctx.ui.notify(`Memory loaded: ${content.length} characters`, "info");
        }
      } catch (error) {
        ctx.ui.notify(`Failed to load memory: ${(error as Error).message}`, "error");
      }
    }
  });

  // Inject memory into system prompt before each agent turn
  pi.on("before_agent_start", async (event, ctx) => {
    if (!memoryContent) return;

    // Prepend memory to the system prompt. Using a clear header so the model
    // understands this is persistent context from the memory file.
    const memorySection = `\n\n# Memory\n\n${memoryContent}\n`;
    
    return {
      systemPrompt: event.systemPrompt + memorySection,
    };
  });
}

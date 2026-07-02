// Alias /exit to the built-in /quit so either command ends the session.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("exit", {
    description: "Exit pi (alias for /quit)",
    handler: async (_args: string, ctx: any) => {
      // ctx.shutdown() requests a graceful shutdown, deferred until the agent is idle.
      if (typeof ctx?.shutdown === "function") {
        ctx.shutdown();
      }
    },
  });
}

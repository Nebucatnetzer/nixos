// Task list for the modes extension — pi's equivalent of Claude Code's TodoWrite.
//
// Adapted from the upstream examples/extensions/todo.ts. The key idea is kept: state lives in
// the tool result's `details`, not in an external file, so /tree branching and /resume restore
// the list that was correct at that point in history for free. Upstream's 60-line TUI overlay
// component is dropped — index.ts renders the list as an always-visible widget, so a separate
// full-screen view would show the same thing twice.

import { StringEnum } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";

export interface TodoItem {
  id: number;
  text: string;
  done: boolean;
}

interface TodoDetails {
  todos: TodoItem[];
  nextId: number;
  error?: string;
}

const TodoParams = Type.Object({
  action: StringEnum(["add", "toggle", "list", "clear"] as const),
  text: Type.Optional(Type.String({ description: "Step text (for add)" })),
  id: Type.Optional(Type.Number({ description: "Step id (for toggle)" })),
});

export interface TodoStore {
  getItems: () => TodoItem[];
}

/** Registers the `todo` tool and returns a live view of the list for the widget. */
export function registerTodo(pi: ExtensionAPI, onChange: () => void): TodoStore {
  let todos: TodoItem[] = [];
  let nextId = 1;

  // Replay the tool results on the current branch to rebuild state. Each result carries the
  // full list, so the last one wins.
  const reconstruct = (ctx: ExtensionContext): void => {
    todos = [];
    nextId = 1;
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type !== "message") continue;
      const message = entry.message;
      if (message.role !== "toolResult" || message.toolName !== "todo") continue;
      const details = message.details as TodoDetails | undefined;
      if (details?.todos) {
        todos = details.todos;
        nextId = details.nextId;
      }
    }
    onChange();
  };

  pi.on("session_start", async (_event, ctx) => reconstruct(ctx));
  pi.on("session_tree", async (_event, ctx) => reconstruct(ctx));

  const snapshot = (error?: string): TodoDetails => ({
    todos: todos.map((todo) => ({ ...todo })),
    nextId,
    ...(error ? { error } : {}),
  });

  const render = (): string =>
    todos.length
      ? todos.map((todo) => `[${todo.done ? "x" : " "}] #${todo.id} ${todo.text}`).join("\n")
      : "No steps yet";

  pi.registerTool({
    name: "todo",
    label: "Todo",
    description:
      "Track the steps of a multi-step task. Call with action=add once per step when you start " +
      "planning work, then action=toggle with the step id as each step is finished. Use " +
      "action=list to re-read the list and action=clear to start over. Keep step text short.",
    parameters: TodoParams,

    async execute(_toolCallId, params) {
      switch (params.action) {
        case "add": {
          if (!params.text) {
            return { content: [{ type: "text", text: "Error: text required for add" }], details: snapshot("text required") };
          }
          const item: TodoItem = { id: nextId++, text: params.text, done: false };
          todos.push(item);
          onChange();
          return { content: [{ type: "text", text: `Added #${item.id}: ${item.text}` }], details: snapshot() };
        }

        case "toggle": {
          if (params.id === undefined) {
            return { content: [{ type: "text", text: "Error: id required for toggle" }], details: snapshot("id required") };
          }
          const item = todos.find((todo) => todo.id === params.id);
          if (!item) {
            return {
              content: [{ type: "text", text: `Step #${params.id} not found` }],
              details: snapshot(`#${params.id} not found`),
            };
          }
          item.done = !item.done;
          onChange();
          return {
            content: [{ type: "text", text: `#${item.id} ${item.done ? "done" : "reopened"}\n\n${render()}` }],
            details: snapshot(),
          };
        }

        case "clear": {
          const count = todos.length;
          todos = [];
          nextId = 1;
          onChange();
          return { content: [{ type: "text", text: `Cleared ${count} step(s)` }], details: snapshot() };
        }

        default:
          return { content: [{ type: "text", text: render() }], details: snapshot() };
      }
    },

    renderCall(args, theme) {
      let line = theme.fg("toolTitle", theme.bold("todo ")) + theme.fg("muted", args.action);
      if (args.text) line += ` ${theme.fg("dim", `"${args.text}"`)}`;
      if (args.id !== undefined) line += ` ${theme.fg("accent", `#${args.id}`)}`;
      return new Text(line, 0, 0);
    },

    renderResult(result, _opts, theme) {
      const details = result.details as TodoDetails | undefined;
      if (details?.error) return new Text(theme.fg("error", `Error: ${details.error}`), 0, 0);
      const first = result.content[0];
      return new Text(theme.fg("muted", first?.type === "text" ? first.text : ""), 0, 0);
    },
  });

  pi.registerCommand("todos", {
    description: "Show the current task list",
    handler: async (_args, ctx) => ctx.ui.notify(render(), "info"),
  });

  return { getItems: () => todos.map((todo) => ({ ...todo })) };
}

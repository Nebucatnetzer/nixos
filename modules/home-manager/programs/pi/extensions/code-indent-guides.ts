// Draw indentation guides inside fenced code blocks. pi has no built-in guide feature and
// no theme token for one, so we inject the guide characters into the markdown itself via
// registerMarkdownTransformer (pi >= 0.84.0). The hook is display only: the message stored
// in the session keeps its original text.
//
// Consequence of doing this as a text transform: the guides are part of the code pi
// highlights, so they land in whatever token class the tokenizer picks (normally
// syntaxPunctuation, which is grey in alabaster-light), and selecting code in the terminal
// copies them along with it.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const GUIDE = "│";
const TAB_WIDTH = 4;
const DEFAULT_UNIT = 4;
const MIN_UNIT = 2;
const MAX_UNIT = 8;

// 3+ backticks or tildes, optionally indented, per CommonMark.
const FENCE_RE = /^(\s*)(`{3,}|~{3,})(.*)$/;

type MarkdownContext = {
  messageType: "user" | "assistant" | "assistant-thinking";
  isStreaming: boolean;
  availableWidth: number;
};

// Expand leading tabs to the next tab stop and return the line with a pure-space indent.
function expandLeadingTabs(line: string): string {
  let column = 0;
  let index = 0;
  while (index < line.length && (line[index] === " " || line[index] === "\t")) {
    column += line[index] === "\t" ? TAB_WIDTH - (column % TAB_WIDTH) : 1;
    index++;
  }
  return " ".repeat(column) + line.slice(index);
}

function leadingSpaces(line: string): number {
  let count = 0;
  while (count < line.length && line[count] === " ") {
    count++;
  }
  return count;
}

// Smallest positive indent in the block. Simple to explain and right for the common cases
// (4 for Python, 2 for JSON/YAML/Nix); a stray alignment indent can throw it off, hence
// the clamp.
function detectUnit(lines: string[]): number {
  let unit = 0;
  for (const line of lines) {
    if (line.trim() === "") {
      continue;
    }
    const indent = leadingSpaces(line);
    if (indent > 0 && (unit === 0 || indent < unit)) {
      unit = indent;
    }
  }
  if (unit === 0) {
    return DEFAULT_UNIT;
  }
  return Math.min(MAX_UNIT, Math.max(MIN_UNIT, unit));
}

function addGuides(block: string[]): string[] {
  const expanded: string[] = [];
  for (const line of block) {
    expanded.push(expandLeadingTabs(line));
  }

  const unit = detectUnit(expanded);
  const level = GUIDE + " ".repeat(unit - 1);

  const guided: string[] = [];
  for (const line of expanded) {
    if (line.trim() === "") {
      guided.push(line);
      continue;
    }
    const indent = leadingSpaces(line);
    const levels = Math.floor(indent / unit);
    const remainder = indent % unit;
    guided.push(level.repeat(levels) + " ".repeat(remainder) + line.slice(indent));
  }
  return guided;
}

function transform(markdown: string): string {
  const lines = markdown.split("\n");
  const out: string[] = [];

  let block: string[] | null = null;
  let marker = "";

  for (const line of lines) {
    const fence = FENCE_RE.exec(line);

    if (block === null) {
      if (fence) {
        block = [];
        marker = fence[2];
      }
      out.push(line);
      continue;
    }

    // A closing fence uses the same character, is at least as long, and carries no info
    // string. Anything else is block content, including shorter fences.
    const closes =
      fence !== null &&
      fence[2][0] === marker[0] &&
      fence[2].length >= marker.length &&
      fence[3].trim() === "";

    if (closes) {
      out.push(...addGuides(block), line);
      block = null;
      continue;
    }

    block.push(line);
  }

  // Unterminated fence: emit the collected lines untouched rather than guessing.
  if (block !== null) {
    out.push(...block);
  }

  return out.join("\n");
}

export default function (pi: ExtensionAPI) {
  pi.registerMarkdownTransformer((markdown: string, ctx: MarkdownContext) => {
    // Streaming updates can hold a half-open fence, and thinking blocks stay plain.
    if (ctx.isStreaming || ctx.messageType !== "assistant") {
      return markdown;
    }
    return transform(markdown);
  });
}

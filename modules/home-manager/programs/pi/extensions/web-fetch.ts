// Minimal web_fetch tool so pi can read public web pages while in read-only mode
// (it neither edits nor writes). Dependency-free: global fetch + a light HTML strip.
// `bash` + curl remain a fallback, but a first-class tool renders and truncates cleanly.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

const MAX_BYTES = 50_000;
const MAX_LINES = 2000;

function htmlToText(html: string): string {
  return html
    .replace(/<script\b[^>]*>[\s\S]*?<\/script>/gi, " ")
    .replace(/<style\b[^>]*>[\s\S]*?<\/style>/gi, " ")
    .replace(/<!--[\s\S]*?-->/g, " ")
    .replace(/<br\s*\/?>/gi, "\n")
    .replace(/<\/(p|div|section|article|li|tr|h[1-6])>/gi, "\n")
    .replace(/<[^>]+>/g, " ")
    .replace(/&nbsp;/gi, " ")
    .replace(/&amp;/gi, "&")
    .replace(/&lt;/gi, "<")
    .replace(/&gt;/gi, ">")
    .replace(/&quot;/gi, '"')
    .replace(/&#39;/gi, "'")
    .replace(/[ \t]+\n/g, "\n")
    .replace(/\n{3,}/g, "\n\n")
    .replace(/[ \t]{2,}/g, " ")
    .trim();
}

function truncate(text: string): { text: string; truncated: boolean } {
  let out = text;
  let truncated = false;
  const lines = out.split("\n");
  if (lines.length > MAX_LINES) {
    out = lines.slice(0, MAX_LINES).join("\n");
    truncated = true;
  }
  if (out.length > MAX_BYTES) {
    out = out.slice(0, MAX_BYTES);
    truncated = true;
  }
  return { text: out, truncated };
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "web_fetch",
    label: "Web Fetch",
    description:
      "Fetch a public URL over HTTP(S) and return its readable text content. " +
      "Use for reading documentation, issues, or reference pages.",
    promptSnippet: "web_fetch: read a public URL as text",
    promptGuidelines: [
      "Use web_fetch to read documentation or reference pages from the internet.",
      "Pass an absolute http(s) URL. Output is stripped to text and truncated.",
    ],
    parameters: Type.Object({
      url: Type.String({ description: "Absolute http(s) URL to fetch" }),
    }),
    async execute(
      _toolCallId: string,
      params: any,
      signal: AbortSignal,
      onUpdate: any,
    ) {
      const url: string = params.url;
      if (!/^https?:\/\//i.test(url)) {
        return {
          content: [
            {
              type: "text",
              text: `Invalid URL (must start with http:// or https://): ${url}`,
            },
          ],
        };
      }
      onUpdate?.({ content: [{ type: "text", text: `Fetching ${url} …` }] });
      try {
        const response = await fetch(url, {
          signal,
          redirect: "follow",
          headers: { "user-agent": "pi-web-fetch/1.0" },
        });
        const contentType = response.headers.get("content-type") ?? "";
        const raw = await response.text();
        const body = /html/i.test(contentType) ? htmlToText(raw) : raw;
        const { text, truncated } = truncate(body);
        const header =
          `# ${url}\n` +
          `HTTP ${response.status} ${response.statusText} · ` +
          `${contentType || "unknown"}${truncated ? " · [truncated]" : ""}\n\n`;
        return {
          content: [{ type: "text", text: header + text }],
          details: { url, status: response.status, truncated },
        };
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return {
          content: [{ type: "text", text: `Failed to fetch ${url}: ${message}` }],
        };
      }
    },
  });
}

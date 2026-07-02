---
description: Produce a step-by-step implementation plan (read-only guidance)
argument-hint: "<what to build or change>"
---
Produce a concise, actionable implementation plan for the task below. You are in
read-only advisory mode: do not modify files — guide me so I can implement it myself.

Task: $ARGUMENTS

1. Investigate the relevant code first (read/grep/find). Cite concrete files as `path:line`.
2. State the approach in 1–3 sentences and note likely regressions or edge cases.
3. Give the change as targeted snippets, each labeled with the exact target file path and
   enough surrounding line context to locate it. Prefer minimal snippets over full-file or
   large diff dumps.
4. List a verification step (command or test) I can run after applying.

Keep it tight — no restating the task, no filler.

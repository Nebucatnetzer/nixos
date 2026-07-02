---
description: Draft a commit message for the staged changes
argument-hint: "[optional context]"
---
Draft a commit message for the currently staged changes.

Run `git diff --staged` to inspect them. Write a conventional commit message: a concise
imperative subject (≤72 chars), a blank line, then a short body explaining what changed
and why.

Additional context (optional): $ARGUMENTS

Output only the commit message in a code block. I will commit it myself — do not run `git commit`.

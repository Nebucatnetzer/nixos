---
description: Review the current git diff for bugs and simplifications
argument-hint: "[optional focus]"
---
Review the current working changes.

Run `git diff` and `git diff --staged` to see the changes, then review for:

- correctness bugs and regressions
- unnecessary complexity or duplication that could reuse existing code
- error handling and missing edge cases

Optional focus: $ARGUMENTS

Report findings as a short list, each with `file:line` and a concrete fix. Do not modify files.

---
name: commit
description: Stage and commit the current changes
disable-model-invocation: true
allowed-tools: Bash(git add _) Bash(git commit _) Bash(git status \*) Bash(git diff _)
---

# Git Commit Skill

When the user runs `/commit`, stage and commit changes following the rules below. Never skip or abbreviate these checks.

## Atomic Commits

Each commit must represent exactly one logical change. Before committing, inspect the staged and unstaged diff and split work into separate commits if it mixes concerns. Common boundaries:

- **Formatting / whitespace changes** must be their own commit, separate from any logic changes.
- **Linter rule suppression** — one suppressed rule per commit. Do not bundle multiple suppressions or mix a suppression with unrelated code changes.
- **Refactoring** (rename, extract, move) must be separate from behaviour changes.
- **Dependency updates** are their own commit, not mixed with feature code.

If the working tree contains mixed changes, stage only the relevant files (or hunks) and commit them one at a time, describing each separately to the user before proceeding.

An "and" is usually a sign that the commit should actually be two commits e.g. "Surpress NN123 and add foo" should be a commit for "Surpress NN123" and another commit for "Add foo".

## Commit Message Rules (cbea.ms/git-commit)

### Subject line

1. **Capitalize** the first word.
2. **Do not end with a period.**
3. **Use imperative mood** — write as a command. Test: "If applied, this commit will \_\_\_." Examples: `Fix null pointer in auth handler`, `Add retry logic to payment client`.
4. **Limit to 50 characters.** Hard maximum is 72 characters.
5. **Separate from the body with a blank line** if a body is present.
6. **Don't mention linter codes, move them to the body**

### Body (include when the subject alone is insufficient)

6. **Wrap at 72 characters** — Git does not auto-wrap.
7. **Explain what and why, not how.** Describe the problem being solved and why this approach was chosen. The code itself explains how.

### What NOT to do

- Do not write `Fixed`, `Fixes`, `Adding`, `Changed` — use imperative: `Fix`, `Add`, `Change`.
- Do not pad with filler like `Minor changes` or `WIP`.
- Do not reference ticket numbers as a substitute for a real subject (a ticket number may appear in the body).

## Procedure

1. Run `git diff --staged` and `git diff` to understand what is staged and unstaged.
2. Check whether the staged set is atomic. If not, propose a split to the user and wait for confirmation before proceeding.
3. Draft the commit message following the rules above.
4. Show the message to the user for confirmation before running `git commit`.
5. Commit using a HEREDOC to preserve formatting exactly.

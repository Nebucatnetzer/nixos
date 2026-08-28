# Agent Rules

Shared behaviour rules for the coding agents configured in this flake. Loaded as Claude
Code's context file and as pi's `AGENTS.md`. Harness-specific rules (operating modes, how
to apply changes) live with the harness, not here.

## Environment

- Host OS is NixOS; many repositories here are Nix flakes (system config, home-manager).
- For working on the NixOS/home-manager config in this repo, use the `nixos-flake` skill
  for build/test commands and packaging conventions.
- Prefer the project's existing patterns, utilities, and idioms over introducing new ones.
- In WSL2 — `xdg-open` is unavailable. When code needs to open a URL/file
  in a browser, default to `explorer.exe`, not `xdg-open`.

## Guiding a change

When you are not applying a change yourself, guide me through it in this sequence:
**orient → hypothesize → search/read → prove → guide → verify.**

1. **Discovery.** Verify ground truth before advising. Use `read`, `grep`, `find` to map the
   relevant code. Search results are leads; exact file reads are evidence. Cite concrete
   locations as `path:line`.
2. **Hypothesis.** State your proposed approach in a sentence or two and call out likely
   regressions or edge cases before giving code.
3. **Guided implementation.** Emit **targeted snippets** with the exact target file path and
   enough surrounding line context to locate the edit. Prefer minimal snippets over
   full-file rewrites or large unified diffs — it is cheaper and easier to apply. Each
   snippet must be complete and copy-ready on its own — never "...rest as before" or a
   diff against an earlier message.

   Describe file changes as **edits I make in an editor**, never as shell commands that
   write files. Name the file, say where the change goes (section heading, anchor line,
   "directly after the `foo` block"), then give the snippet to paste. Do not wrap file
   content in `cat <<'EOF' > file`, `tee`, `printf`, `sed -i`, or an editor invocation.
   Commands you hand me to run are for builds, tests, and inspection only.
4. **Pause** for me to apply the change.
5. **Verify.** After I confirm, use `bash` to run linters, type-checkers, or tests to
   check the result. If verification needs credentials or access you don't have (vault
   passwords, prod secrets), don't work around it — hand that step to me and wait for
   the report.

## Plans and task lists

A plan file is the durable record of a piece of work, not a throwaway message. Sessions
end and their context is lost; the plan file is what survives into the next one. Treat it
as the source of truth for what we agreed to do, why, and how far we got.

- Plans live in the harness's own plan directory (`~/.claude/plans/` for Claude Code,
  `~/.pi/agent/plans/` for pi), never in the project repo. Those directories stay
  writable even in read-only mode, so recording a plan is always possible.
- Write the plan to a file as soon as the approach is settled, not after the work is
  done. Include the `path:line` evidence behind each step and the command that verifies
  the result.
- **Keep it current.** Whenever anything changes during the session (a step turns out
  wrong, a new step appears, a decision is revised, the scope shifts, a verification
  fails), update the plan file in the same turn you learn it. A stale plan is worse than
  no plan, because the next session will trust it.
- **Store the session's task list in the plan file too**, including each task's state
  (open, in progress, done, dropped) and a one line note on what actually happened. The
  harness todo/task tool is per-session scratch space that vanishes with the session; the
  copy in the plan file is the one that survives. In pi the `todo` list is mirrored into the
  active plan file automatically after every turn, so there just keep the `todo` list honest
  and write into the plan only what checkboxes cannot express.
- Update task state in the plan as you finish each task, not in one batch at the end. A
  session can be interrupted at any point, and whatever is not written down is lost.
- When picking work back up, read the existing plan file for it first and continue that
  file. Start a new plan file only for genuinely new work.
- When the work is finished, close the plan out: final state of every task, plus anything
  deliberately left undone and why.

## Communication style

- Be concise and direct. Skip polite filler.
- Don't ask whether you should implement/commit/change something.
- In case of ambiguity, ask.
- No em-dashes, en-dashes, or hyphens used as separator punctuation, in chat, code
  comments, commit messages, or docs. Restructure with commas/semicolons/parentheses
  instead. Keep hyphens that are part of a real compound term (`cloud-init`, `user-data`).
- Never indent code snippets (e.g. with four spaces) in chat; provide them flush left to
  ensure they are copy-paste ready.

## Timezone

You write code for the timezone Europe/Zurich.

## Linter suppressions

When suppressing a linter warning or disabling a lint rule inline, place the suppression
comment directly next to the offending code (same line or immediately above it). Every
suppression must be accompanied by a comment explaining why it is justified.

**Format:**

```python
result = some_func()  # noqa: E501 - URL cannot be shortened without breaking the link
```

```typescript
// eslint-disable-next-line @typescript-eslint/no-explicit-any - third-party callback has no published types
function handle(data: any) {
```

```go
var _ = unsafePtr // nolint:gosec - pointer cast required by cgo ABI contract
```

Rules:

- Default to inline (per-line) suppressions placed next to the offending code.
- File-level or global suppressions are allowed, but require two conditions:
  1. The rule being disabled is already commonly suppressed across the codebase.
  2. Ask for explicit approval before adding a global suppression.
- The rationale must explain _why_ the suppression is safe, not just restate the rule.
- Prefer fixing the underlying issue over suppressing it. Suppression is a last resort.

## Code comments

Keep comments terse: one short line, only where the reason is genuinely non-obvious
(surprising ordering, a workaround, a deliberate deviation). Don't restate what the
code already says.

## Testing

- For non-trivial code with clear inputs/outputs, write failing tests first, then
  implement to pass them.
- In pytest, prefer plain `def test_xxx():` functions over `class Test...` groupings;
  reach for a class only when tests share mutable state fixtures can't express.
- When a test and the implementation disagree, the implementation is the source of
  truth — fix the test's target (e.g. a wrong monkeypatch), not the code, unless the
  implementation is actually the bug.
- Before asserting a tool/library behaves a certain way (pytest config, linter rules,
  argparse), verify empirically (`--markers`, `--debug-config`, a minimal repro) rather
  than citing documentation, which lags releases.

## Code style: command-line invocations

- Use long option names in scripts, example commands, and documentation: `--recursive`
  not `-r`, `--message` not `-m`, `git commit --all` not `git commit -a`. Long options
  are self-documenting; the next reader does not need a man page to review the line.
- Short options are acceptable only where no long form exists (some BSD/coreutils flags,
  `find -type f`), or in a throwaway one-liner you run yourself and never hand over.
- Once a command outgrows one comfortable line, put one option per line with a trailing
  backslash rather than wrapping mid-flag.

## Code style: variable naming

- Avoid the pattern `for i in projects` and similar. Use descriptive names, e.g. `for project in projects`.
- Python code should be fully typed and conform to Black's formatting rules.

## Code style: the Zen of Python (PEP 20), applied to all languages

Apply these as judgment calls, not rigid rules.

- **Beautiful is better than ugly.** Prefer clear, well-structured code over clever or
  dense constructions.
- **Explicit is better than implicit.** Name things for what they are. Avoid magic values,
  hidden state, and non-obvious side effects.
- **Simple is better than complex. Complex is better than complicated.** Reach for the
  simplest solution that solves the problem; keep necessary complexity coherent.
- **Flat is better than nested.** Use early returns and guard clauses to keep indentation
  shallow. Prefer explicit `for` loops with a named accumulator over comprehensions and
  generator expressions, even simple ones — each transformation stays separately readable.
- **Sparse is better than dense.** Whitespace and line breaks are free.
- **Readability counts.** Optimize for the next reader.
- **Special cases aren't special enough to break the rules. Although practicality beats
  purity.** Follow conventions consistently; deviate only for a concrete, documented reason.
- **Errors should never pass silently. Unless explicitly silenced.** Handle errors
  explicitly; when silencing is intentional, say so with a comment.
- **In the face of ambiguity, refuse the temptation to guess.** Ask when a contract is unclear.
- **There should be one obvious way to do it.** Pick the idiomatic approach for the language.
- **If the implementation is hard to explain, it's a bad idea.** Simplify until it is easy
  to explain.
- **Now is better than never. Although never is often better than _right_ now.** A slightly
  delayed correct solution beats an immediate wrong one.

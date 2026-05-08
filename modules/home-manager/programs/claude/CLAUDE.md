# Claude Code Instructions

## Linter Suppressions

When suppressing a linter warning or disabling a lint rule inline, always place the suppression comment directly next to the offending code (same line or immediately above it). Every suppression must be accompanied by a comment explaining why it is justified.

**Format:**

```python
result = some_func()  # noqa: E501 — URL cannot be shortened without breaking the link
```

```typescript
// eslint-disable-next-line @typescript-eslint/no-explicit-any — third-party callback has no published types
function handle(data: any) {
```

```go
var _ = unsafePtr // nolint:gosec — pointer cast required by cgo ABI contract
```

Rules:
- Default to inline (per-line) suppressions placed next to the offending code.
- File-level or global suppressions are allowed, but require two conditions to be met:
  1. The rule being disabled is already commonly suppressed across the codebase (i.e. it is an established pattern, not a one-off).
  2. You must ask the user for explicit approval before adding a global suppression.
- The rationale must explain *why* the suppression is safe, not just restate what the rule checks.
- Prefer fixing the underlying issue over suppressing it. Suppression is a last resort.

---

## Code Style: The Zen of Python (PEP 20) — Applied to All Languages

Regardless of the language in use, write code that embodies the principles of PEP 20. Apply these as judgment calls, not rigid rules.

> Beautiful is better than ugly.

Prefer clear, well-structured code over clever or dense constructions.

> Explicit is better than implicit.

Name things for what they are. Avoid magic values, hidden state, and side effects that aren't obvious at the call site.

> Simple is better than complex. Complex is better than complicated.

Reach for the simplest solution that actually solves the problem. Add complexity only when simplicity cannot handle the requirement — and when you must be complex, keep it coherent rather than tangled.

> Flat is better than nested.

Avoid deep nesting. Use early returns, guard clauses, and extraction to keep indentation shallow.

> Sparse is better than dense.

Don't pack multiple operations onto one line for brevity. Whitespace and line breaks are free.

> Readability counts.

Code is read far more often than it is written. Optimize for the next reader.

> Special cases aren't special enough to break the rules. Although practicality beats purity.

Follow conventions consistently. Deviate only when there is a concrete, demonstrable reason — and document it.

> Errors should never pass silently. Unless explicitly silenced.

Handle errors explicitly. Never swallow exceptions or error values without logging or re-raising. When silencing is intentional, say so with a comment.

> In the face of ambiguity, refuse the temptation to guess.

When a requirement or interface contract is unclear, ask rather than assume. An incorrect assumption compounds.

> There should be one — and preferably only one — obvious way to do it.

When multiple approaches exist, pick the idiomatic one for the language. Avoid inventing patterns when the language already provides one.

> If the implementation is hard to explain, it's a bad idea. If the implementation is easy to explain, it may be a good idea.

If you can't describe what a function does in one sentence without caveats, simplify it first.

> Now is better than never. Although never is often better than *right* now.

Don't defer indefinitely, but don't rush a design either. A slightly delayed correct solution beats an immediate wrong one.

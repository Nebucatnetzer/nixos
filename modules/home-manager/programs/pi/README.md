# pi coding agent — custom setup

Home-manager module that packages [pi](https://pi.dev) (`pi-coding-agent`) as a
**read-only advisory agent by default**, with a Claude-Code-style permission-mode cycle:
plan → read-only → edit. It also adds a task list, a plan-file artifact, slash commands,
skills shared with Claude Code, internet reads, and live cost/model info from the Infomaniak API.

pi's core ships deliberately minimal (no built-in MCP or sub-agents), but it does ship working
reference implementations of plan mode and a todo tool under
`examples/extensions/` in its own store path. The `modes` extension here is adapted from those
rather than written from scratch — see the comments in `extensions/modes/` for what was changed
and why.

Behaviour rules, the `/commit` command and the skills are shared with Claude Code in
`../ai/` so they are maintained once. See `../ai/RULES.md`.

## Layout

| Path                     | Purpose                                                                                         |
| ------------------------ | ----------------------------------------------------------------------------------------------- |
| `default.nix`            | Wires everything: settings, secret, and `home.file` drops into `~/.pi/agent/`                   |
| `pi-coding-agent.nix`    | Trimmed copy of the upstream home-manager module (**leave as-is**; drop when it lands upstream) |
| `pi_wrapper.nix`         | `bwrap` sandbox around the `pi` binary                                                          |
| `keybindings.json`       | Moves `app.thinking.cycle` off `shift+tab` so the mode cycle can claim it                       |
| `MEMORY.md`              | Seed for `~/.pi/agent/MEMORY.md` (copied once, then user-writable; `memory` option)             |
| `context-prune-settings.json` | Seed for `~/.pi/agent/context-prune/settings.json` (copied once, then user-writable)        |
| `extensions/*.ts`        | TypeScript extensions (see below)                                                               |
| `extensions/modes/`      | Permission modes, bash guard, `todo` tool, `plan` tool                                          |
| `prompts/*.md`           | Slash commands                                                                                  |
| `../ai/RULES.md`         | Shared behaviour rules — pi's `AGENTS.md`, and part of Claude Code's `CLAUDE.md`                 |
| `../ai/commands/`        | Slash commands shared with Claude Code (`/commit`)                                              |
| `../ai/skills/`          | Skills shared with Claude Code                                                                  |

All resources are written under `~/.pi/agent/`, which pi auto-discovers and which the sandbox
binds **writable** — that is where the plan files and `MEMORY.md` live.

## Permission modes

Cycle with **`shift+tab`**; the footer shows the current mode and task progress.

| Mode                | `edit`/`write` | Behaviour                                                              |
| ------------------- | -------------- | ---------------------------------------------------------------------- |
| `◔ plan`            | blocked        | Investigate only, no code. Records a plan file + populates the task list |
| `◎ read-only`       | blocked        | **Default.** Guides one step at a time; you apply the changes           |
| `✎ EDIT`            | allowed        | Applies changes directly. No per-change prompts                        |

- **`shift+tab`** cycles; **F2** or **`/edit`** jumps straight to edit and back; **`/plan`**
  enters plan mode; **`/mode [plan\|advise\|edit]`** shows or sets it.
- Start in a mode with **`pi --plan`** or **`pi --write`**.
- The mode survives `/resume` and branches with `/tree` (persisted as a session entry).
- **Bash guardrail:** outside edit mode, file-mutating shell commands (`sed -i`, `>`/`>>` to a
  real file, `tee`, `patch`, `git apply/add/commit/…`, `rm`/`mv`/`cp`, …) plus `sudo`,
  `systemctl`, package installs and interactive editors are blocked, so the model can't bypass
  the disabled tools. Read-only shell (`git diff/log/status`, `nix build`, tests, linters,
  `>/dev/null`) passes through. This is a denylist on purpose: upstream's allowlist has no
  `nix`/`pytest`/`cargo`/`make` entries and would block the verification step.

Writes are gated **in-app**, not by the sandbox: `pi_wrapper.nix` binds `$PWD` writable, so
the extension is the real gate. There are deliberately no per-tool-call approval prompts.

### Plan → guide handoff

In plan mode, once pi records a plan with the `plan` tool it asks what to do next: *walk me
through it* (switches to read-only and starts on step 1), *apply it yourself* (switches to
edit), or *stay in plan mode*. This is the equivalent of Claude Code's ExitPlanMode.

Plans are written to `~/.pi/agent/plans/<timestamp>-<slug>.md` — not into the repo, since
`~/.pi` is writable in every mode and plans don't belong in project history. The last 20 are
kept.

Implemented in `extensions/modes/`.

## Extensions

| File                   | Adds                                                                        |
| ---------------------- | --------------------------------------------------------------------------- |
| `modes/index.ts`       | Permission modes, bash guardrail, progress widget, plan handoff             |
| `modes/utils.ts`       | The destructive-command denylist (pure; unit-testable)                      |
| `modes/todo.ts`        | `todo` tool + `/todos` — task list, state stored in tool-result `details`   |
| `modes/plan.ts`        | `plan` tool — writes the plan file under `~/.pi/agent/plans/`               |
| `web-fetch.ts`         | `web_fetch` tool — read a public URL as text (works in read-only mode)      |
| `exit-alias.ts`        | `/exit` as an alias for `/quit`                                             |
| `infomaniak-models.ts` | Registers the Infomaniak provider dynamically; adds `/rates`                |
| `infomaniak-cost.ts`   | Session-cost footer + `/bill` command                                       |
| `claude-context.ts`    | Injects a project's `CLAUDE.md` into context (checks `cwd` only)            |
| `load-memory.ts`       | Appends `~/.pi/agent/MEMORY.md` to the system prompt                        |

## Slash commands & skill

- `/plan` — enter plan mode (a *mode*, not a template — hence no `prompts/plan.md`)
- `/mode [plan|advise|edit]` — show or set the permission mode
- `/edit` — toggle edit mode
- `/todos` — show the current task list
- `/review [focus]` — review the current `git diff`
- `/commit [context]` — draft a commit message from the staged diff
- `/explain <file|symbol>` — explain code
- `/skill:nixos-flake` — build/test conventions for this flake (shared with Claude Code)
- `/rates` — per-model price (CHF per 1M tokens)
- `/bill` — month-to-date Infomaniak spend vs the CHF 20 budget

## Models, pricing & cost

The Infomaniak provider is **not** hard-coded. `infomaniak-models.ts` fetches, at startup:

- `GET /2/ai/{product}/openai/v1/models` — available model ids (embedding/rerank filtered out),
- `GET /1/ai/models` — real context window (`max_token_input`),
- `GET /1/ai/{product}/consumptions` — per-model spend + tokens.

Infomaniak exposes no rate card, so per-token cost is **derived from real usage history**
(spend ÷ tokens, blended input+output). This feeds pi's live session-cost footer and `/rates`.
`models.json` is written as `{ providers = {} }` only to satisfy pi's schema — the real
provider is registered at runtime.

To change the product id / budget / token path, edit the constants at the top of the two
`infomaniak-*.ts` files. The API token is the agenix secret `infomaniakAiToken`.

## Token-utility extensions (pinned)

Three third-party extensions by [championswimmer](https://github.com/championswimmer),
pinned in `default.nix` via `fetchFromGitHub` and loaded through `settings.packages` store
paths — reproducible and sandbox-safe, unlike imperative `pi install`. All are
dependency-free (peer deps only), so no `node_modules` build. `pi-context-prune` goes
through a `runCommandLocal` that applies two patches, both of which abort the build if
their anchor disappears on a version bump:

1. It imports `@sinclair/typebox`, which pi bundles as `typebox`, so the specifier is
   rewritten.
2. Its summarizer calls `provider.stream` without `maxRetries`, and pi-ai's
   `retryProviderRequest` reads that as zero retries, so one 429 from Infomaniak aborts
   the prune. `maxRetries: 4` is injected to match the main loop, see below.

| Package            | Adds                                                                             | Notes                                                                    |
| ------------------ | -------------------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `pi-context-prune` | Prunes verbose tool outputs from future context; `context_tree_query` recovers   | Configured only via its own settings file, see below                     |
| `pi-context-usage` | `/context` — context-window breakdown (system prompt / tools / messages / free)  | provider-agnostic                                                       |
| `pi-cache-graph`   | `/cache graph\|stats\|export` — prompt-cache hit stats                            | works here because Infomaniak reports `cached_tokens`                    |

To bump a version, change the `rev` in the `let` block of `default.nix`, set the matching
`hash = pkgs.lib.fakeHash`, rebuild once, and paste the reported hash back.

### Pruner configuration

`pi-context-prune` ignores pi's `settings.json` entirely; it reads and rewrites
`~/.pi/agent/context-prune/settings.json` (its `/pruner` subcommands write that file).
`context-prune-settings.json` is therefore copied there once on activation, the same
copy-once-then-writable treatment as `MEMORY.md`, and `/pruner` stays usable afterwards.

`batchingMode` is set to `agent-message` on purpose. With the upstream default (`turn`),
every assistant turn becomes its own batch, so a turn holding one short tool result
(tens of chars) gets a summary that is necessarily longer than the output it replaces.
The pruner then throws that summary away, warns `skipped pruning turn N ... frontier
advanced past this range`, and the summarizer call is billed for nothing. Merging a whole
user-to-final-answer span into one batch gives the summary enough raw text to beat.

Per-turn batching also fires one summarizer request per pending turn, all in flight at
once (`summarizeBatches` uses an unbounded `Promise.all`), which is a burst Infomaniak
answers with 429s. Merged batching means one request per user turn instead.

To change it at runtime: `/pruner batching agent-message`, `/pruner prune-on <mode>`,
`/pruner status`. Runtime changes persist in the copied file and survive rebuilds; edit
`context-prune-settings.json` for a fresh machine.

## Settings (token frugality)

Set in `default.nix`: `quietStartup`, `defaultThinkingLevel = "low"`,
`compaction.enabled`, `enableSkillCommands`. Default model is gemma 4 31B.

## Rate limits

Infomaniak returns 429 under load. Three layers handle it, and only the first is on by
default:

- **Session retry** (`retry.enabled`, default true, 3 attempts, 2s/4s/8s): pi drops the
  failed assistant turn and replays it. It works, but it resends the whole context, so a
  429 storm gets expensive.
- **Provider retry** (`retry.provider.maxRetries = 4`, set in `default.nix`): retries
  inside the request, honouring `retry-after`/`retry-after-ms` with jittered backoff and
  no token cost. Off upstream, since `retry.provider.*` has no default and pi-ai reads
  `maxRetries ?? 0`. Requests still fail immediately when the server asks for a delay
  above `retry.provider.maxRetryDelayMs` (60s default).
- **Summarizer retry**: the pruner bypasses both of the above, so it is patched in the
  `runCommandLocal`, see the pinned-extensions section.

## Sandbox

`pi_wrapper.nix` runs pi under `bwrap`: root filesystem read-only, `$PWD` and `~/.pi`
bound writable, network open (needed for the API + `web_fetch`), TS cache in the `~/.cache`
tmpfs. `$PWD` is writable so the in-app mode gate is the thing that decides — note `~/.claude`
is *not* bound, so Claude Code's skills are not reachable from inside pi's sandbox. Extensions must import only bundled modules (`@earendil-works/pi-*`, `typebox`,
`node:*`) — no npm install happens in the sandbox.

## Adding resources

Add a file under `prompts/`, `../ai/skills/<name>/`, or `extensions/`, then register it in the
`home.file` block of `default.nix`. Extensions are plain TypeScript loaded via jiti (type
annotations are stripped at runtime); a multi-file extension goes in a subdirectory with an
`index.ts`. Import only bundled modules (`@earendil-works/pi-*`, `typebox`, `node:*`).

The pure parts of `extensions/modes/` can be exercised without a rebuild:

```
node --experimental-strip-types <test.ts>   # with pi's node_modules resolvable
```

## Rebuild & verify

pi is enabled through `modules/home-manager/profiles/desktop.nix`, so rebuild the host
(e.g. capricorn):

```
sudo nixos-rebuild switch --flake .#capricorn
```

Then check: `pi --list-models` (context sizes attached), `/rates` (non-zero for used models),
`shift+tab` cycles the three modes, a mutating bash command is blocked outside edit mode but
`nix build` is not, `/plan` produces a file under `~/.pi/agent/plans/`, and the mode survives
`pi --continue`.

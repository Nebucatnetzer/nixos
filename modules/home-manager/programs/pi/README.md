# pi coding agent — custom setup

Home-manager module that packages [pi](https://pi.dev) (`pi-coding-agent`) as a
**read-only advisory agent**: by default it explains changes and you apply them yourself,
with a one-key toggle to let it edit when you want. It also adds Claude-Code-style slash
commands, a skill, internet reads, and live cost/model info sourced from the Infomaniak API.

pi ships deliberately minimal (no MCP, sub-agents, plan mode, or modes); everything below is
assembled from pi's own primitives — prompt templates, skills, extensions, and context files.

## Layout

| Path                     | Purpose                                                                                         |
| ------------------------ | ----------------------------------------------------------------------------------------------- |
| `default.nix`            | Wires everything: settings, secret, and `home.file` drops into `~/.pi/agent/`                   |
| `pi-coding-agent.nix`    | Trimmed copy of the upstream home-manager module (**leave as-is**; drop when it lands upstream) |
| `pi_wrapper.nix`         | `bwrap` sandbox around the `pi` binary; also implements `pi --write`                            |
| `APPEND_SYSTEM.md`       | Global behavioral rules + the advisory workflow (appended to the system prompt)                 |
| `AGENTS.md`              | Environment/context facts only                                                                  |
| `extensions/*.ts`        | TypeScript extensions (see below)                                                               |
| `prompts/*.md`           | Slash commands                                                                                  |
| `skills/<name>/SKILL.md` | Skills (progressive disclosure)                                                                 |

All resources are written under `~/.pi/agent/`, which pi auto-discovers and which the
sandbox binds read-only.

## Read-only default & edit toggle

- `edit`/`write` tools are disabled at startup, so pi guides instead of editing.
- Press **F2** or run **`/edit`** to toggle edit mode (footer shows `◎ read-only` / `✎ EDIT`).
- Start directly in edit mode with **`pi --write`** (sets `PI_EDIT_MODE=1`).
- **Bash guardrail:** while read-only, file-mutating shell commands (`sed -i`, `>`/`>>` to a
  real file, `tee`, `patch`, `git apply/add/commit/…`, `rm`/`mv`/`cp`, …) are blocked so the
  model can't bypass the disabled tools. Read-only shell (`git diff/log/status`, tests,
  linters, builds, `>/dev/null`) passes through. In edit mode the guard is off.

Implemented in `extensions/edit-mode.ts`.

## Extensions

| File                   | Adds                                                                   |
| ---------------------- | ---------------------------------------------------------------------- |
| `edit-mode.ts`         | Read-only default, F2/`/edit` toggle, bash mutation guardrail          |
| `web-fetch.ts`         | `web_fetch` tool — read a public URL as text (works in read-only mode) |
| `exit-alias.ts`        | `/exit` as an alias for `/quit`                                        |
| `infomaniak-models.ts` | Registers the Infomaniak provider dynamically; adds `/rates`           |
| `infomaniak-cost.ts`   | Session-cost footer + `/bill` command                                  |

## Slash commands & skill

- `/plan <task>` — step-by-step implementation plan (read-only guidance)
- `/review [focus]` — review the current `git diff`
- `/commit [context]` — draft a commit message from the staged diff
- `/explain <file|symbol>` — explain code
- `/skill:nixos-flake` — build/test conventions for this flake (`nix build .#azPkgs.<name>`, etc.)
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
dependency-free (peer deps only), so no `node_modules` build. `pi-context-prune` imports
`@sinclair/typebox`, which pi bundles as `typebox`, so a small `runCommandLocal` rewrites
that specifier.

| Package            | Adds                                                                             | Notes                                                                    |
| ------------------ | -------------------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `pi-context-prune` | Prunes verbose tool outputs from future context; `context_tree_query` recovers   | `piContextPrune.pruneOn = "agent-message"` keeps the cache prefix stable |
| `pi-context-usage` | `/context` — context-window breakdown (system prompt / tools / messages / free)  | provider-agnostic                                                       |
| `pi-cache-graph`   | `/cache graph\|stats\|export` — prompt-cache hit stats                            | works here because Infomaniak reports `cached_tokens`                    |

To bump a version, change the `rev` in the `let` block of `default.nix`, set the matching
`hash = pkgs.lib.fakeHash`, rebuild once, and paste the reported hash back.

## Settings (token frugality)

Set in `default.nix`: `quietStartup`, `defaultThinkingLevel = "low"`,
`compaction.enabled`, `enableSkillCommands`. Default model is gemma 4 31B.

## Sandbox

`pi_wrapper.nix` runs pi under `bwrap`: root filesystem read-only, only `$PWD` writable,
`~/.pi` bound, network open (needed for the API + `web_fetch`), TS cache in the `~/.cache`
tmpfs. Extensions must import only bundled modules (`@earendil-works/pi-*`, `typebox`,
`node:*`) — no npm install happens in the sandbox.

## Adding resources

Add a file under `prompts/`, `skills/<name>/`, or `extensions/`, then register it in the
`home.file` block of `default.nix`. Extensions are plain TypeScript loaded via jiti (type
annotations are stripped at runtime).

## Rebuild & verify

pi is enabled through `modules/home-manager/profiles/desktop.nix`, so rebuild the host
(e.g. capricorn):

```
sudo nixos-rebuild switch --flake .#capricorn
```

Then check: `pi --list-models` (context sizes attached), `/rates` (non-zero for used
models), F2 toggles read-only ↔ edit, and a mutating bash command is blocked while read-only.

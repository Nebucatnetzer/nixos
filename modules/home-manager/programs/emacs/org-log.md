# Project log — dated entries per project, aggregated on demand

Append a dated line to any project (or to no project at all) in a few keystrokes, and keep a
running log of the last two weeks on screen next to the agenda.

Implemented in `emacs.d/lib/az_org_log.el`. Tests in `emacs.d/lib/tests/az_org_log-tests.el`.

## The idea in one paragraph

A project's log lives **inside its own denote note**, under a `* Log` heading, so it archives
together with the project when `C-c C-$` moves the file to `99_archive/`. Entries that don't
belong to a project yet go to the **denote journal**. The chronological cross-project view is
**generated on demand** and never stored — so there is nothing to keep in sync and no tagging
discipline to maintain.

## Daily use

| Key       | Does                                                                            |
| --------- | ------------------------------------------------------------------------------- |
| `C-c c l` | Add a log entry. Prompts for a destination, then drops you in a capture buffer. |
| `<f11>`   | Show or hide the aggregated log panel on the right.                             |
| `C-c n d` | Dashboard: the `A` agenda view in the main window, log panel beside it.         |

The destination prompt lists `(unassigned)` first, then your projects newest first:

```
Log to: (unassigned)
        motorradtour 2026
        archival software
        pebble development
        ...
```

Pick `(unassigned)` for anything without a home yet — a stray thought, a task you want to flesh
out later. It lands in today's journal file and still shows up in the panel. Promote it to a
project later with `m` (see below); nothing is lost by not deciding up front.

Project names come from each file's `#+title:`, falling back to the denote file name.

### Multi-line entries and sub-tasks

The capture buffer is a normal org buffer, so an entry can be as long as you like:

```
- SDK needs python2; found a nixpkgs override
  the override lives in pkgs/pebble-sdk
  - [ ] try the emulator
  - [ ] check the strap fits
```

`M-RET` starts a new item, `M-<right>` demotes it into a sub-task, `C-c C-c` toggles a checkbox.
Finish with `C-c C-c`. The whole thing stays one entry and appears complete in the panel.

## What a project file ends up looking like

```org
#+title: Pebble development

* Tasks
** NEXT Get the SDK building

* Log
** [2026-08-25 Tue]
- read the SDK docs
- needs python2; found a nixpkgs override
  - [ ] try the emulator
** [2026-08-24 Mon]
- ordered a replacement strap
```

Newest day on top, so opening a project shows you where you left off. New entries append under
the current day. `* Log` is created on first use — **existing projects need no migration**.

The day heading is an inactive timestamp (`[...]`, no time). Org only puts _active_ `<...>`
timestamps in the agenda, so the log is invisible there. That is deliberate: unchecked `- [ ]`
boxes inside log entries stay out of your task views too. If a sub-task turns out to matter,
promote it to `* Tasks` as a real `NEXT`.

## The panel

```
2026-08-25 Tuesday

  (unassigned)
  - idea: script the RAW import
  pebble development
  - read the SDK docs
  - needs python2; found a nixpkgs override
    - [ ] try the emulator

2026-08-24 Monday

  pebble development
  - ordered a replacement strap
```

Days descending, unassigned first within a day. Keys inside the panel:

| Key       | Does                                                                 |
| --------- | -------------------------------------------------------------------- |
| `RET`     | Jump to that entry in its source file                                |
| `C-c C-c` | Toggle the checkbox on this line, in the source file, then re-render |
| `m`       | Move this entry into a project, keeping its original date            |
| `r`       | Rebuild                                                              |
| `q`       | Close the panel                                                      |

The panel refreshes itself after every capture. It shows `az-org-log-days` days back — 14 by
default; lower it if entries routinely run long.

`m` works on the entry as a whole: invoke it on a nested sub-task and the parent entry moves
with all its children, not just that line. It keeps the entry's original date rather than
today's, creates the day heading in the target if needed, and removes the source day heading
when nothing is left under it. `org-refile` can't do any of this — log entries are plain list
items, not headings.

## Configuration

| Variable                      | Default           | Meaning                        |
| ----------------------------- | ----------------- | ------------------------------ |
| `az-org-log-days`             | `14`              | How far back the panel reaches |
| `az-org-log-heading`          | `"Log"`           | Heading entries live under     |
| `az-org-log-buffer`           | `"*Project Log*"` | Panel buffer name              |
| `az-org-log-unassigned-label` | `"(unassigned)"`  | Label for journal entries      |

Paths all derive from `az_paths.el`. Which means the whole thing re-roots with one line.

## Using it at work

`init.el` loads `~/.emacs.d/variables.el` **before** `modules.el`, and `az_paths.el` declares
its paths with `defvar` — which does not overwrite an already-bound value. So on a second
machine, put this in that machine's `~/.emacs.d/variables.el`:

```elisp
(setq az-nextcloud-dir "~/notes/")
(setq enable-org t)
(setq enable-notes t)
```

That re-roots the inbox, projects dir, archive, journal and `denote-directory` in one go. No
change to this repo is needed. Create `01_inbox/` and `02_projects/` under the new root.
`variables.el` is hand-copied, not nix-managed.

## Notes for future maintenance

Some non-obvious constraints the code depends on. Breaking any of these fails quietly rather
than loudly.

- **Never hand a file path to an org function that visits it.** `org-refile-targets` is built
  from `az-org-files-list`, which returns _currently open_ org buffers. `org-get-title "path"`
  and `find-file-noselect` both add buffers, so calling either across every project would grow
  your refile completions as a side effect. The aggregator reads bytes into a temp buffer
  instead.
- **`az-org-log--project-files` is depth-limited** to files directly in `02_projects/` or one
  level below. There is an old journal tree nested inside the `reorganising-my-notes` project;
  an unbounded recursive search sweeps up dozens of 2023/2024 entries as if they were projects.
- **The aggregator globs `99_archive/*/journal/`, never `99_archive/` itself.** Archived
  projects live under `99_archive/<year>/projects/` and carry their own `* Log` trees, which
  would otherwise reappear in the panel after you archived them.
- **All years are globbed**, not just the current one, so the 14-day window doesn't truncate
  every January.
- **Day headings are compared by date only** (`2026-08-25`), not by the whole stamp. A locale
  change in the day name would otherwise produce two headings for one day.
- **`denote-journal-directory` is recomputed before use.** `az_denote.el` builds it with
  `format-time-string` at load time, so a daemon running past New Year would write into the
  previous year. `az-org-log--sync-journal-directory` is advised onto the journal entry points,
  which fixes `C-c n t` as well.
- **The capture template is `plain`, not `item`.** `item` does list-aware insertion and
  repositions away from the append point the target function picked.

## Running the tests

```sh
cd modules/home-manager/programs/emacs/emacs.d/lib
emacs -Q --batch -l tests/az_org_log-tests.el -f ert-run-tests-batch-and-exit
```

The test file stubs `denote-journal` and the path variables, so it runs without the package set.

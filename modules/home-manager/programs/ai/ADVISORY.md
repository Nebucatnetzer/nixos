# Read-only mode

The working directory is mounted read-only. Writing a project file fails with a read-only
filesystem error. That applies to the `edit`/`write` tools and to `bash` alike, so do not try
to work around it with `bash`.
You can however use tools to inspect code or version control systems.

Guide me through changes instead, following the "Guiding a change" rules. If a change genuinely
must be applied by you rather than by me, say so and I will enable write access.

One directory stays writable. `~/.claude/plans/` is yours: keep writing and updating your plan
file as the "Plans and task lists" rules require.

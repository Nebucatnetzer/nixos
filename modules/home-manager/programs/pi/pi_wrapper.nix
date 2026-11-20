{
  pi-coding-agent,
  bubblewrap,
  git,
  writeShellApplication,
}:
writeShellApplication {
  name = "pi";
  runtimeInputs = [
    pi-coding-agent
    bubblewrap
    git
  ];
  text = ''
    # Agent, bus, tmux and WSL sockets give a way out of the sandbox.
    unset CONTRIA_VAULT_PASS SSH_AUTH_SOCK DBUS_SESSION_BUS_ADDRESS \
      TMUX TMUX_PANE WSL_INTEROP WSLENV

    project_root=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
    case "$project_root" in
      "$HOME" | /)
        echo "pi: start it inside a project, not in $project_root" >&2
        exit 1
        ;;
    esac

    # /run/WSL exists only on WSL. bwrap cannot create a missing mount point under the read-only root.
    wsl_tmpfs=()
    if [ -d /run/WSL ]; then
      wsl_tmpfs=(--tmpfs /run/WSL)
    fi

    # `--write` and `--plan` are real pi flags, registered by the modes extension via
    # pi.registerFlag, so they pass straight through rather than being intercepted here.
    #
    # The project root is bound writable on purpose: the modes extension is the write gate,
    # and a --ro-bind would make that toggle dead code.
    #
    # Mount order matters: the read-only git paths must come after the project bind.
    bwrap \
      --ro-bind / / \
      --tmpfs /mnt \
      "''${wsl_tmpfs[@]}" \
      --tmpfs "/run/user/$(id --user)" \
      --dev /dev \
      --proc /proc \
      --tmpfs /tmp \
      --tmpfs "$HOME" \
      --tmpfs "$HOME/.cache" \
      --tmpfs "$HOME/.config" \
      --bind "$project_root" "$project_root" \
      --ro-bind-try "$project_root/.git/config" "$project_root/.git/config" \
      --ro-bind-try "$project_root/.git/hooks" "$project_root/.git/hooks" \
      --bind "$HOME/.pi" "$HOME/.pi" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --chdir "$PWD" \
      --unshare-pid \
      --unshare-ipc \
      --unshare-uts \
      --unshare-cgroup-try \
      --die-with-parent \
      pi "$@"
  '';
}

{
  claude-code,
  bubblewrap,
  writeShellApplication,
}:
writeShellApplication {
  name = "claude";
  runtimeInputs = [
    claude-code
    bubblewrap
  ];
  text = ''
    unset CONTRIA_VAULT_PASS;

    # claude-code neutralizes project-local config by mounting /dev/null over these paths. A
    # mount needs its target to exist, so claude-code creates the missing ones itself, which
    # fails once $PWD is read-only and then its bash tool stops working. Create them here
    # instead and remove them again on exit. Mounting over a file needs no write permission,
    # so read-only $PWD is fine once they exist.
    claude_config_nodes=(
      .bash_profile
      .bashrc
      .gitconfig
      .gitmodules
      .idea
      .mcp.json
      .profile
      .ripgreprc
      .vscode
      .zprofile
      .zshrc
    )
    created_nodes=()
    for node in "''${claude_config_nodes[@]}"; do
      if [ ! -e "$PWD/$node" ]; then
        : > "$PWD/$node"
        created_nodes+=("$PWD/$node")
      fi
    done

    # Remove only the empty placeholders this wrapper made, never a real file.
    cleanup_created_nodes() {
      for node in ''${created_nodes[@]+"''${created_nodes[@]}"}; do
        if [ -f "$node" ] && [ ! -s "$node" ]; then
          rm --force "$node"
        fi
      done
    }
    trap cleanup_created_nodes EXIT

    # claude-code keeps its own state in .claude/.cc-writes, so that one stays writable.
    mkdir --parents "$PWD/.claude"

    # $PWD is bound read-only, so writes to project files fail with EROFS. A read-only bind
    # stays a view of the same filesystem, so edits made outside the session appear at once.
    # Do not replace this with --tmp-overlay: overlayfs caches the name to inode mapping, so a
    # file replaced by rename, which is what editors do, keeps showing its old content.
    #
    # .git is writable because claude-code creates .git/config.lock to stop `git config` from
    # writing. The worktree stays read-only, so a commit can only record what is already on
    # disk.
    bwrap \
      --ro-bind / / \
      --tmpfs /mnt/ \
      --dev /dev \
      --proc /proc \
      --tmpfs /tmp \
      --tmpfs "$HOME" \
      --tmpfs "$HOME/.cache" \
      --tmpfs "$HOME/.config" \
      --ro-bind "$PWD" "$PWD" \
      --bind "$PWD/.claude" "$PWD/.claude" \
      --bind-try "$PWD/.git" "$PWD/.git" \
      --bind "$HOME/.claude" "$HOME/.claude" \
      --bind "$HOME/.claude.json" "$HOME/.claude.json" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --unshare-pid \
      --die-with-parent \
      claude "$@"
  '';
}

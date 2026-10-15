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
    bwrap \
      --ro-bind / / \
      --tmpfs /mnt/ \
      --dev /dev \
      --proc /proc \
      --tmpfs /tmp \
      --tmpfs "$HOME" \
      --tmpfs "$HOME/.cache" \
      --tmpfs "$HOME/.config" \
      --bind "$PWD" "$PWD" \
      --bind "$HOME/.claude" "$HOME/.claude" \
      --bind "$HOME/.claude.json" "$HOME/.claude.json" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --unshare-pid \
      --die-with-parent \
      claude "$@"
  '';
}

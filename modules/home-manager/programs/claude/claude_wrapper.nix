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
      --ro-bind /mnt/c/Users/"$USER"/.claude/ /mnt/c/Users/"$USER"/.claude/ \
      --ro-bind /mnt/wsl /mnt/wsl \
      --dev /dev \
      --proc /proc \
      --tmpfs /tmp \
      --tmpfs "$HOME" \
      --tmpfs "$HOME/.cache" \
      --tmpfs "$HOME/.config" \
      --ro-bind "$HOME"/.nix-profile "$HOME"/.nix-profile \
      --bind "$PWD" "$PWD" \
      --bind "$HOME/.claude" "$HOME/.claude" \
      --bind "$HOME/.claude.json" "$HOME/.claude.json" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --unshare-pid \
      --die-with-parent \
      claude "$@"
  '';
}

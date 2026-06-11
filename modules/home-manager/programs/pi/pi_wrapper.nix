{
  pi-coding-agent,
  bubblewrap,
  writeShellApplication,
}:
writeShellApplication {
  name = "pi";
  runtimeInputs = [
    pi-coding-agent
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
      --bind "$HOME/.pi" "$HOME/.pi" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --unshare-pid \
      --die-with-parent \
      pi "$@"
  '';
}

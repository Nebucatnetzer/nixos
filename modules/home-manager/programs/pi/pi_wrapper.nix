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
    # `pi --write` starts in edit mode; the edit-mode extension reads PI_EDIT_MODE.
    # (pi has no --write flag of its own, so intercept it here.) bwrap inherits the env.
    if [ "''${1:-}" = "--write" ]; then
      export PI_EDIT_MODE=1
      shift
    fi
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

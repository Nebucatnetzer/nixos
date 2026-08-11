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
    # `--write` and `--plan` are real pi flags, registered by the modes extension via
    # pi.registerFlag, so they pass straight through rather than being intercepted here.
    #
    # $PWD is bound writable on purpose: the modes extension is the write gate, and a
    # --ro-bind would make that toggle dead code.
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

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
    # `--plan` is a real pi flag, registered by the modes extension via pi.registerFlag, so it
    # passes straight through rather than being intercepted here.
    #
    # $PWD is bound read-only. pi writes nothing into the working directory, so it needs no
    # writable exception. Do not replace this with --tmp-overlay: overlayfs caches the name to
    # inode mapping, so a file replaced by rename keeps showing its old content.
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
      --bind "$HOME/.pi" "$HOME/.pi" \
      --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
      --unshare-pid \
      --die-with-parent \
      pi "$@"
  '';
}

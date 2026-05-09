{
  lib,
  netcat,
  openssh,
  writeShellScriptBin,
}:
lib.meta.addMetaAttrs
  {
    description = "Wait for a host's initrd SSH to become available then connect to unlock LUKS";
    license = lib.licenses.gpl3Plus;
    mainProgram = "unlock-luks";
    platforms = lib.platforms.linux;
  }
  (
    writeShellScriptBin "unlock-luks" ''
      until ${netcat}/bin/nc -vzw 2 $1 22; do
          sleep 1
      done &&
          ${openssh}/bin/ssh \
            -o UserKnownHostsFile=/dev/null \
            -o StrictHostKeyChecking=no \
            -o User=root \
            $1
    ''
  )

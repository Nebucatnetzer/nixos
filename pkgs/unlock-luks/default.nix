{
  netcat,
  openssh,
  writeShellScriptBin,
}:
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

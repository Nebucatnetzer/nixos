{
  OVMF,
  qemu,
  writeShellApplication,
}:
writeShellApplication {
  name = "test-image";
  runtimeInputs = [ qemu ];
  text = ''
    if [ -z "$1" ]; then
      echo "Usage: $0 <path-to-boot-image>"
      exit 1
    fi
    tmpFile=$(mktemp /tmp/test-image.XXXXXX)
    trap 'rm -f $tmpFile' EXIT
    cp "$1" "$tmpFile"
    qemu-system-x86_64 \
      -enable-kvm \
      -m 8G \
      -cpu max \
      -smp 2 \
      -netdev user,id=net0,hostfwd=tcp::2222-:22 \
      -device virtio-net-pci,netdev=net0 \
      -drive if=pflash,format=raw,readonly=on,file=${OVMF.firmware} \
      -drive if=pflash,format=raw,readonly=on,file=${OVMF.variables} \
      -drive "if=virtio,format=raw,file=$tmpFile"
  '';
}

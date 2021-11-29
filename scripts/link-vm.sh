rm -f /mnt/etc/nixos/configuration.nix
rm -f /mnt/etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/vm/configuration.nix /mnt/etc/nixos/configuration.nix



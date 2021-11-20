rm /mnt/etc/nixos/configuration.nix
rm /mnt/etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/vm/configuration.nix /mnt/etc/nixos/configuration.nix



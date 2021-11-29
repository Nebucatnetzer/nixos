rm /mnt/etc/nixos/configuration.nix
rm /mnt/etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/xps5530/configuration.nix /mnt/etc/nixos/configuration.nix

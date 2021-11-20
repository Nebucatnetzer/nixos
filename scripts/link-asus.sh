rm /mnt/etc/nixos/configuration.nix
rm /mnt/etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/asus/configuration.nix /mnt/etc/nixos/configuration.nix


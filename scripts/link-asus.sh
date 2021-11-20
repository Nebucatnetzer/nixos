rm -f /etc/nixos/configuration.nix
rm -f /etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/asus/configuration.nix /etc/nixos/configuration.nix


rm /etc/nixos/configuration.nix
rm /etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/xps5530/configuration.nix /etc/nixos/configuration.nix

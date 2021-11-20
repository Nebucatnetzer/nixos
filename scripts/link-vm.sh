rm /etc/nixos/configuration.nix
rm /etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/vm/configuration.nix /etc/nixos/configuration.nix



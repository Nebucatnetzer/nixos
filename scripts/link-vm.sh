rm -f /etc/nixos/configuration.nix
rm -f /etc/nixos/hardware-configuration.nix

ln -s $(pwd)/hardware/vm/configuration.nix /etc/nixos/configuration.nix



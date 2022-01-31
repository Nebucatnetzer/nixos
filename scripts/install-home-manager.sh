#!/usr/bin/env bash
nix-shell '<home-manager>' -A install
rm ~/.config/nixpkgs/home.nix
ln -s $(pwd)/.nixos/flake.nix /home/$USER/.config/nixpkgs/flake.nix
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.con
home-manager switch

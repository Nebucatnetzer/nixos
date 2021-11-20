nix-shell '<home-manager>' -A install
rm ~/.config/nixpkgs/home.nix
rm -r ~/.config/qtile
ln -s $(pwd)/home-manager/$ZWEILI_ENVIRONMENT.nix /home/$USER/.config/nixpkgs/home.nix
home-manager switch

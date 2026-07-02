# 1. install Nix in single user mode
# external profile script, not present at build time
# shellcheck source=/dev/null
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >~/.config/nix/nix.conf
nix-shell '<home-manager>' -A install
rm ~/.config/nixpkgs/home.nix
ln -s "$HOME/.nixos/flake.nix" "$HOME/.config/nixpkgs/flake.nix"
home-manager switch |& nom

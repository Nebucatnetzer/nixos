{ ... }:
{
  # Inspired by
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/minimal.nix
  environment.noXlibs = true;
  documentation.enable = false;
  documentation.nixos.enable = false;
  programs.command-not-found.enable = false;
}

{ config, pkgs, ... }:
let
  unstable = import
    (builtins.fetchGit {
      # Descriptive name to make the store path easier to identify
      url = "https://github.com/nixos/nixpkgs/";
      # Commit hash for nixos-unstable as of 2018-09-12
      # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
      ref = "refs/heads/nixos-unstable";
      rev = "942eb9a335b4cd22fa6a7be31c494e53e76f5637";
    }) # reuse the current configuration
    {
      config = config.nixpkgs.config;
    };
in
{
  home.packages = with pkgs; [
    unstable.obsidian
  ];
}

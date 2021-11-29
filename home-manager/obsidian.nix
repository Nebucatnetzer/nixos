{ config, pkgs, ... }:
let
  unstable = import
    (builtins.fetchTarball {
      # Descriptive name to make the store path easier to identify
      url = "https://github.com/nixos/nixpkgs/archive/942eb9a335b4cd22fa6a7be31c494e53e76f5637.tar.gz";
      # Commit hash for nixos-unstable as of 2018-09-12
      # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
      sha256 = "05390093gl44h8v6pgklwkgbn3vwdhs81shabqmjagq6rg1sh1l5";
    })
    # reuse the current configuration
    {
      config = config.nixpkgs.config;
    };
in
{
  home.packages = with pkgs; [
    unstable.obsidian
  ];
}

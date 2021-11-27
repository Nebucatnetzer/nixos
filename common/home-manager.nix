{ config, pkgs, ... }:
let
  system-version = import ../version.nix;
  home-manager-url = "https://github.com/nix-community/home-manager/archive/release-" + system-version + ".tar.gz";
  home-manager = builtins.fetchTarball home-manager-url;
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  home-manager.users.andreas = {
    imports = [
      ../home-manager/desktop.nix
    ];
  };
}

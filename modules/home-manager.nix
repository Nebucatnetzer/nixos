{ ... }:
let
  system-version = import ../version.nix;
  home-manager-url = "https://github.com/nix-community/home-manager/archive/release-" + system-version + ".tar.gz";
  home-manager = builtins.fetchTarball home-manager-url;
  username = import ../username.nix;
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.users.${username} = {
    imports = [
      ../home-manager/desktop.nix
    ];
  };
}

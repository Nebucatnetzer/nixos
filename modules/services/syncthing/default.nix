{
  dataRoot,
  exposeWebInterface ? false,
}:
{
  config,
  lib,
  ...
}:
let
  homeDirectory = "/home/${config.az-username}";
in
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = config.az-username;
    dataDir = "${homeDirectory}/.config/syncthing";
    guiAddress = lib.mkIf exposeWebInterface "0.0.0.0:8384";
    settings = import ./settings.nix {
      azHosts = config.az-hosts;
      hostname = config.networking.hostName;
      inherit dataRoot homeDirectory lib;
    };
  };
  networking.firewall.interfaces.wg0.allowedTCPPorts = lib.optional exposeWebInterface 8384;
}

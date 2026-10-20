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
  # A root outside the home directory (gwyn) must exist and belong to the Syncthing user.
  systemd.tmpfiles.rules = lib.optional (
    !lib.hasPrefix homeDirectory dataRoot
  ) "d ${dataRoot} 0700 ${config.az-username} ${config.az-username} -";
  networking.firewall.interfaces.wg0.allowedTCPPorts = lib.optional exposeWebInterface 8384;
}

{
  exposeWebInterface ? false,
}:
{
  config,
  lib,
  ...
}:
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = config.az-username;
    dataDir = "/home/${config.az-username}/.config/syncthing";
    guiAddress = (lib.mkIf exposeWebInterface "0.0.0.0:8384");
  };
  networking.firewall.interfaces.wg0.allowedTCPPorts = [
    (lib.mkIf exposeWebInterface 8384)
  ];
}

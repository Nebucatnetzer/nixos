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
  hostname = config.networking.hostName;
  homeDirectory = "/home/${config.az-username}";
  hostFolders = lib.filterAttrs (name: folder: lib.elem hostname folder.devices) (
    import ./folders.nix
  );
  # The host stays in the list, otherwise syncthing-init deletes its own device entry.
  devices = lib.unique (lib.concatMap (folder: folder.devices) (lib.attrValues hostFolders));
  folderPath =
    name: folder:
    if folder ? homePath then
      "${homeDirectory}/${folder.homePath}"
    else
      "${dataRoot}/${folder.path or name}";
in
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = config.az-username;
    dataDir = "${homeDirectory}/.config/syncthing";
    guiAddress = lib.mkIf exposeWebInterface "0.0.0.0:8384";
    settings = {
      devices = lib.genAttrs devices (device: {
        id = config.az-hosts.${device}.syncthingId;
      });
      folders = lib.mapAttrs (name: folder: {
        inherit (folder) id;
        label = name;
        path = folderPath name folder;
        type = folder.type or "sendreceive";
        devices = lib.remove hostname folder.devices;
      }) hostFolders;
    };
  };
  networking.firewall.interfaces.wg0.allowedTCPPorts = lib.optional exposeWebInterface 8384;
}

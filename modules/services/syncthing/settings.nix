# Builds services.syncthing.settings for one host from folders.nix and hosts.nix.
{
  azHosts,
  dataRoot,
  homeDirectory,
  hostname,
  lib,
}:
let
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
  devices = lib.genAttrs devices (
    device:
    let
      host = azHosts.${device};
    in
    {
      id = host.syncthingId;
      # Try the WireGuard address first; dynamic covers LAN discovery and hosts without wg.
      addresses = lib.optional (host ? wgIp) "tcp://${host.wgIp}:22000" ++ [ "dynamic" ];
    }
  );
  folders = lib.mapAttrs (name: folder: {
    inherit (folder) id;
    label = name;
    path = folderPath name folder;
    type = folder.type or "sendreceive";
    devices = lib.remove hostname folder.devices;
  }) hostFolders;
}

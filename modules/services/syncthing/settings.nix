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
  devices = lib.genAttrs devices (device: {
    id = azHosts.${device}.syncthingId;
  });
  folders = lib.mapAttrs (name: folder: {
    inherit (folder) id;
    label = name;
    path = folderPath name folder;
    type = folder.type or "sendreceive";
    devices = lib.remove hostname folder.devices;
  }) hostFolders;
}

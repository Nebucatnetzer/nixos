{
  mountPath ? "/",
}:
{ pkgs, ... }:
{
  boot.supportedFilesystems = [
    "btrfs"
  ];

  environment.systemPackages = [
    pkgs.compsize # required to display additional information about btrfs compression
  ];
  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [
        mountPath
      ];
      interval = "monthly";
    };
  };
}

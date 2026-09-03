{
  # A list, not a single path: importing this module twice would collide on
  # services.btrfs.autoScrub.fileSystems instead of merging.
  mountPaths ? [ "/" ],
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
      fileSystems = mountPaths;
      interval = "monthly";
    };
  };
}

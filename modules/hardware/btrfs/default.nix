{
  filesystemName ? "root",
  beesSpec ? "/",
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
    beesd = {
      filesystems = {
        "${filesystemName}" = {
          extraOptions = [
            "--loadavg-target"
            "2.0"
            "--thread-factor"
            "0.5"
          ];
          spec = beesSpec;
        };
      };
    };
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [
        beesSpec
      ];
      interval = "monthly";
    };
  };
}

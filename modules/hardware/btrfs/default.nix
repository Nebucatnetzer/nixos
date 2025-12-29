{
  btrfsLabel ? "mainBtrfs",
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
        root = {
          extraOptions = [
            "--loadavg-target"
            "2.0"
            "--thread-factor"
            "0.5"
          ];
          spec = "/";
        };
      };
    };
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [
        "/"
      ];
      interval = "monthly";
    };
  };
}

{ hostname }: { config, inputs, ... }:
{
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "ohci_pci"
    "sd_mod"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];
  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  networking = {
    hostName = hostname;
    interfaces.enp0s3.useDHCP = true;
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  users.users.${config.az-username} = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCcwq2DNEJyum5Q362dlC97MW9IBJSiL4ZqXmQxdXSCiKvad6QSVic2UE+iBSzpOigB0uqGPH5iCQ2tgnTVB0n3Ttv0FV9Nchu8hEJA6tmdMOvAifVptv4KWU0H7uUI0EV1UewUznofvw67EbbQYhZiT0ydHgjohBBuvrhptME4YBaU74+ZeIDuVfQBRcj2dfvfQKZuJzyRcNg9Ms5XKMkJ2y2Z0KPX9dX91mAHio0grBtck+G2+OlswRmhE67lXoOkeiIqPo158QZEWEakfMCxHBP7KAE6OKevzXcViDksTzVy21LGGFaPvUMDszVal6Gq4WVK0yRjvIqzz66v8QVpsJ7mlsUM3vPbWphNjRNMq7UHcHyI8SYEKtzwC6CFCySMJLZNYPHGP6Q+zsqeSsD9DlxK0i/d5Yzs8YRImqcbdkFc56xJ5s4hd9mP4t4jIuiDR3hXQCIgoO2+a2R0nrRrmw4vD05hnye5+BYFnYLtsvgJGnEhKlmmBpL/T/kcUWk= andreas@co-ws-con4"
    ];
  };
  profiles.az-desktop.enable = true;
  services = {
    az-logs-share.enable = true;
    az-virtualbox-guest.enable = true;
    az-x86.enable = true;
  };
}


{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
{
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cryptd"
    "nvme"
    "rtsx_pci_sdmmc"
    "sd_mod"
    "sr_mod"
    "usbhid"
    "usb_storage"
    "xhci_pci"
  ];

  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "ip=dhcp" # required for ssh at initrd
  ];

  boot.initrd.luks.devices."cryptlvm" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.supportedFilesystems = [
    "apfs"
    "btrfs"
    "exfat"
    "ext4"
    "nfs"
    "nfs4"
    "ntfs"
    "cifs"
    "f2fs"
  ];

  fileSystems."/" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "subvol=root"
      "compress=zstd"
      "noatime"
    ];
  };
  fileSystems."/home" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "subvol=home"
      "compress=zstd"
      "noatime"
    ];
  };
  fileSystems."/nix" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "subvol=swap"
      "compress=zstd"
      "noatime"
    ];
  };
  fileSystems."/swap" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "subvol=swap"
      "noatime"
    ];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  networking.hostName = hostname;

  swapDevices = [ { device = "/swap/swapfile"; } ];

  hardware = {
    az-bluetooth.enable = true;
    az-dvd.enable = true;
    graphics.enable = true;
  };

  profiles.az-desktop.enable = true;
  programs = {
    az-adb.enable = true;
    az-distrobox.enable = true;
    az-makemkv.enable = true;
    az-restic-management.enable = true;
    az-steam.enable = true;
  };

  services = {
    az-binary-cache-client.enable = true;
    az-logs-share.enable = true;
    az-qtile.enable = true;
    az-restic-client-desktop.enable = true;
    az-tlp.enable = true;
    az-x86.enable = true;
    az-zram-swap.enable = true;
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    logind.lidSwitchExternalPower = "ignore";

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

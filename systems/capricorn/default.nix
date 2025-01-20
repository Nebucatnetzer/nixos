{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  commonBtrfsOptions = [
    "compress=zstd"
    "defaults"
    "noatime"
    "ssd"
  ];
in
{
  # Capricorn is a Dell Latitude 7450 with an Intel Core Ultra 7 165U CPU of generation Meteor Lake.
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cryptd"
    "nvme"
    "rtsx_pci_sdmmc"
    "sd_mod"
    "sr_mod"
    "thunderbolt"
    "usbhid"
    "usb_storage"
    "xhci_pci"
  ];
  boot.kernelPackages = pkgs.linuxPackages_6_12;
  boot.initrd.kernelModules = [
    "dm-snapshot"
    "thunderbolt"
    "i915"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "ip=dhcp" # required for ssh at initrd
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.supportedFilesystems = [
    "apfs"
    "btrfs"
    "cifs"
    "exfat"
    "ext4"
    "f2fs"
    "nfs"
    "nfs4"
    "ntfs"
  ];

  fileSystems."/" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=root"
    ] ++ commonBtrfsOptions;
  };
  fileSystems."/home" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=home"
    ] ++ commonBtrfsOptions;
  };
  fileSystems."/nix" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=nix"
    ] ++ commonBtrfsOptions;
  };
  fileSystems."/swap" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "compress=no"
      "noatime"
      "nodatacow"
      "nodatasum"
      "ssd"
      "subvol=swap"
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
    graphics = {
      enable = true;
      extraPackages = [
        pkgs.intel-compute-runtime
        pkgs.intel-media-driver # For Broadwell (2014) or newer processors. LIBVA_DRIVER_NAME=iHD
        pkgs.intel-ocl
        pkgs.libvdpau-va-gl
        pkgs.vpl-gpu-rt
      ];
    };
    ipu6 = {
      enable = true;
      platform = "ipu6epmtl";
    };
  };

  profiles.az-desktop.enable = true;
  programs = {
    az-adb.enable = true;
    az-distrobox.enable = true;
    az-droidcam.enable = true;
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
    logind.lidSwitch = "lock";
    thermald.enable = true;
  };
}

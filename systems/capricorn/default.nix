{ hostname }:
{
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
  toggle-keyboard = pkgs.callPackage "${inputs.self}/pkgs/toggle-keyboard" { };
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
    keyboard.zsa.enable = true;
    # ipu6 = {
    #   enable = true;
    #   platform = "ipu6epmtl";
    # };
  };

  profiles.az-desktop.enable = true;

  environment.systemPackages = [
    pkgs.strawberry # music player
    pkgs.wally-cli # tool to flash a ZSA keyboard
    toggle-keyboard
    pkgs.compsize # required to display additional information about btrfs compression
  ];
  programs = {
    az-adb.enable = true;
    az-distrobox.enable = true;
    az-droidcam.enable = true;
    az-makemkv.enable = true;
    az-restic-management.enable = true;
    az-steam.enable = true;
    localsend = {
      enable = true;
      openFirewall = true;
    };
  };

  services = {
    az-binary-cache-client.enable = true;
    az-media-share.enable = true;
    az-restic-client-desktop.enable = true;
    az-x86.enable = true;
    az-zram-swap.enable = true;
    beesd = {
      filesystems = {
        root = {
          extraOptions = [
            "--loadavg-target"
            "2.0"
            "--thread-factor"
            "0.5"
          ];
          spec = "LABEL=mainBtrfs";
        };
      };
    };

    # required for KDE
    desktopManager.plasma6.enable = true;
    displayManager.sddm.wayland.enable = true;
    displayManager.sddm.enable = true;
    # required for KDE
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    thermald.enable = true;
  };
  # required for KDE
  programs.xwayland.enable = true;
  # required for KDE
}

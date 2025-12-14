{ hostname }:
{
  inputs,
  pkgs,
  ...
}:
let
  commonBtrfsOptions = [
    "compress=lzo"
    "defaults"
    "discard=async" # be explicit about defaults
    "noatime"
    "space_cache=v2" # be explicit about defaults
    "ssd"
  ];
  toggle-keyboard = pkgs.callPackage "${inputs.self}/pkgs/toggle-keyboard" { };
  foxFlss = inputs.fox-flss.packages.${pkgs.stdenv.hostPlatform.system}.default;
  foxFlssWrapper = pkgs.writeShellApplication {
    name = "enable-wwan";
    runtimeInputs = [
      pkgs.networkmanager
    ];
    text = ''
      if [ "$EUID" -ne 0 ]; then
        echo "Please run as root"
        exit
      fi
      mkdir -p /var/log/FoxFlss/Log/
      ${foxFlss}/usr/bin/FoxFlss "$@"
      nmcli radio wwan on
      nmcli connection up yallo
    '';
  };
  mediaShare = import "${inputs.self}/modules/services/media-share" { };
in
{
  imports = [
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/hardware/dvd"
    "${inputs.self}/modules/profiles/desktop"
    "${inputs.self}/modules/programs/adb"
    "${inputs.self}/modules/programs/distrobox"
    "${inputs.self}/modules/programs/droidcam"
    "${inputs.self}/modules/programs/makemkv"
    "${inputs.self}/modules/programs/restic-management"
    "${inputs.self}/modules/programs/steam"
    "${inputs.self}/modules/services/binary-cache-client"
    "${inputs.self}/modules/services/kanata"
    "${inputs.self}/modules/services/kde"
    "${inputs.self}/modules/services/restic-client-desktop"
    "${inputs.self}/modules/services/zram-swap"
    mediaShare
  ];
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
  boot.kernelPackages = pkgs.linuxPackages_6_18;
  boot.kernelPatches = [
    {
      name = "dma-mapping-fix-dma_bit_mask";
      patch = pkgs.fetchpatch {
        url = "https://github.com/torvalds/linux/commit/31b931bebd11a0f00967114f62c8c38952f483e5.patch";
        hash = "sha256-SRVozzu1wc79wG3T0kEVuL3vcWMjN5QDN+llqwjxGlg=";
      };
    }
  ];
  nixpkgs.overlays = [
    (final: prev: {
      libcamera = prev.libcamera.overrideAttrs (oa: rec {
        version = "0.6.0";
        src = pkgs.fetchgit {
          url = "https://git.libcamera.org/libcamera/libcamera.git";
          rev = "v${version}";
          hash = "sha256-zGcbzL1Q2hUaj/s9NjBlp7hVjmSFb0GF8CnCoDS82Tw=";
        };
        buildInputs = oa.buildInputs ++ [
          final.libunwind
        ];
      });
    })
  ];
  boot.initrd.kernelModules = [
    "xe" # graphics driver
    "dm-snapshot"
    "thunderbolt"
    "i915"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "sg"
    "squashfs"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "ip=dhcp" # required for ssh at initrd
    "i915.force_probe=!7d45"
    "xe.force_probe=7d45"
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.supportedFilesystems = [
    "btrfs"
    "cifs"
    "exfat"
    "ext4"
    "f2fs"
    "nfs"
    "nfs4"
    "ntfs"
    "squashfs"
  ];

  fileSystems."/" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=root"
    ]
    ++ commonBtrfsOptions;
  };
  fileSystems."/home" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=home"
    ]
    ++ commonBtrfsOptions;
  };
  fileSystems."/nix" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=nix"
    ]
    ++ commonBtrfsOptions;
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
    ipu6 = {
      enable = true;
      platform = "ipu6epmtl";
    };
  };

  environment.systemPackages = [
    pkgs.compsize # required to display additional information about btrfs compression
    pkgs.wally-cli # tool to flash a ZSA keyboard
    foxFlssWrapper
    toggle-keyboard
  ];
  programs = {
    localsend = {
      enable = true;
      openFirewall = true;
    };
  };

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
          spec = "LABEL=mainBtrfs";
        };
      };
    };
    fprintd.enable = true;
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    thermald.enable = true;
  };
}

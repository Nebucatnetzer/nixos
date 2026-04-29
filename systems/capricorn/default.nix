{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
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
  mediaShare = import "${inputs.self}/modules/services/media-share";
  resticClientModule = import "${inputs.self}/modules/services/restic-client";
  syncthingModule = import "${inputs.self}/modules/services/syncthing";
  wireguardModule = import "${inputs.self}/modules/services/wireguard";
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
    "${inputs.self}/modules/programs/steam"
    "${inputs.self}/modules/services/kanata"
    "${inputs.self}/modules/services/kde"
    "${inputs.self}/modules/services/zram-swap"
    (btrfsAuxModule { })
    (mediaShare { })
    (resticClientModule { })
    (syncthingModule { })
    (wireguardModule {
      IP = config.az-hosts."${hostname}".wgIp;
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
    })
  ];

  age.secrets.wireguardPrivateKey.file = "${inputs.self}/scrts/capricorn_wg.key.age";

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
  boot.kernelPackages = pkgs.linuxPackages_latest;
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
    "i915.force_probe=!7d45"
    "xe.force_probe=7d45"
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

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

  networking.wg-quick.interfaces.wg0.dns = [ config.az-hosts.gwyn.wgIp ];
  networking.hostName = hostname;

  swapDevices = [ { device = "/swap/swapfile"; } ];

  hardware = {
    cpu.intel.npu.enable = true;
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

  environment.systemPackages = [
    pkgs.wally-cli # tool to flash a ZSA keyboard
    foxFlssWrapper
  ];
  programs = {
    kdeconnect.enable = true;
  };

  services = {
    fprintd.enable = true;
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    thermald.enable = true;
  };
}

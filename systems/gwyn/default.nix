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
  domains = [
    { fqdn = "${config.services.librenms.hostname}"; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate";
  resticClientModule = import "${inputs.self}/modules/services/restic-client-desktop";
  syncthingModule = import "${inputs.self}/modules/services/syncthing";
  wireguardModule = import "${inputs.self}/modules/services/wireguard";
in
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/misc/initrd-ssh"
    "${inputs.self}/modules/profiles/management"
    "${inputs.self}/modules/services/coredns"
    "${inputs.self}/modules/services/librenms"
    "${inputs.self}/modules/services/snmpd"
    "${inputs.self}/modules/services/syslog"
    "${inputs.self}/modules/services/wireguard/routing.nix"
    "${inputs.self}/modules/services/zram-swap"
    (btrfsAuxModule { })
    (librenmsCertificateModule { inherit domains; })
    (resticClientModule { resticSchedule = "*-*-* 06..21:30:00"; })
    (syncthingModule { exposeWebInterface = true; })
    (wireguardModule {
      IP = "10.70.89.153";
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
    })
  ];

  age.secrets.wireguardPrivateKey.file = "${inputs.self}/scrts/gwyn_wg.key.age";
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cdc_ether"
    "cryptd"
    "nvme"
    "rtsx_pci_sdmmc"
    "r8152"
    "r8153_ecm"
    "sd_mod"
    "sr_mod"
    "usbhid"
    "usb_storage"
    "xhci_pci"
  ];

  boot.initrd.kernelModules = [
    "cdc_ether"
    "dm-snapshot"
    "r8152"
    "r8153_ecm"
    "xhci_pci"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "rootdelay=10"
    "ip=dhcp" # required for ssh at initrd
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

  fileSystems."/mnt/fileserver/media" = {
    device = "10.7.89.108:media";
    fsType = "nfs";
    options = [
      "hard"
      "noatime"
      "rw"
    ];
  };

  swapDevices = [ { device = "/swap/swapfile"; } ];

  # USB address of the ethernet dongle: 0bda:8153
  networking = {
    useDHCP = false;
    hostName = hostname;
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.1" ];
    interfaces.enp58s0u1.ipv4.addresses = [
      {
        address = "10.7.89.153";
        prefixLength = 24;
      }
    ];
  };

  hardware.graphics.enable = true;

  services = {
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
    thermald.enable = true;

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

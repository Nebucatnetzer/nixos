{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  lib = pkgs.lib;
  domains = [
    { fqdn = "${config.services.librenms.hostname}"; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate" {
    inherit domains;
  };
in
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/programs/restic-management"
    "${inputs.self}/modules/services/binary-cache-client"
    "${inputs.self}/modules/services/data-share"
    "${inputs.self}/modules/services/librenms"
    "${inputs.self}/modules/services/syslog"
    "${inputs.self}/modules/services/zram-swap"
    librenmsCertificateModule
  ];

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

  boot.initrd.luks.devices."cryptlvm" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.supportedFilesystems = [
    "apfs"
    "exfat"
    "ext4"
    "nfs"
    "nfs4"
    "ntfs"
    "cifs"
    "f2fs"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  # USB address of the ethernet dongle: 0bda:8153
  networking = {
    firewall.allowedTCPPorts = [
      3389
    ];
    firewall.allowedUDPPorts = [
      3389
    ];
    useDHCP = false;
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.1" ];
    interfaces.enp58s0u1.ipv4.addresses = [
      {
        address = "10.7.89.153";
        prefixLength = 24;
      }
    ];
  };

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  hardware.graphics.enable = true;

  services = {
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    # todo: has been renamed to `services.logind.settings.Login.HandleLidSwitchExternalPower`.
    logind.lidSwitchExternalPower = "ignore";
    thermald.enable = true;

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

{ hostname }:
{
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.nixos-hardware.nixosModules.dell-precision-5530 ];

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

  networking = {
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

  hardware = {
    az-bluetooth.enable = true;
    az-nvidia.enable = true;
    graphics.enable = true;
  };

  profiles.az-desktop.enable = true;
  programs = {
    az-distrobox.enable = true;
    az-restic-management.enable = true;
  };

  services = {
    az-binary-cache-client.enable = true;
    az-data-share.enable = true;
    az-librenms.enable = true;
    az-restic-client-desktop.enable = true;
    az-x86.enable = true;
    az-zram-swap.enable = true;
    # required for KDE
    desktopManager.plasma6.enable = true;
    displayManager.sddm.wayland.enable = true;
    displayManager.sddm.enable = true;
    # required for KDE
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    logind.lidSwitchExternalPower = "ignore";
    thermald.enable = true;

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

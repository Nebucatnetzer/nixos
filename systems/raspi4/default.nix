{ inputs, hostname, ip, pkgs, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "f2fs";
      options = [ "compress_algorithm=zstd:6"
                  "compress_chksum"
                  "whint_mode=fs-based"
                  "atgc"
                  "gc_merge"
                  "lazytime"
                ];
    };
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  boot.initrd.availableKernelModules = [
    "reset_raspberrypi"
    "sdhci_pci"
  ];
  boot.loader.efi.canTouchEfiVariables = false;
  #boot.loader.grub.enable = true;
  #boot.loader.grub.efiSupport = true;
  #boot.loader.grub.efiInstallAsRemovable = true;
  #boot.loader.grub.device = "nodev";

  hardware.raspberry-pi."4".fkms-3d.enable = true;
  hardware.raspberry-pi."4".audio.enable = true;
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    raspberrypi-eeprom
  ];

  networking = {
    useDHCP = false;
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.2" ];
    interfaces.eth0.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
  };

  environment.shellAliases = {
    raspi-firmware-update = ''
      sudo mount /dev/disk/by-label/FIRMWARE /mnt && \
      BOOTFS=/mnt FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
      sudo umount /mnt
    '';
  };
}

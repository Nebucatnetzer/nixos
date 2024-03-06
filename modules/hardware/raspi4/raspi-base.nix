{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hardware.az-raspi4-base;
  test-sd-card = pkgs.writeShellApplication {
    name = "test-sd-card";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.gawk
      pkgs.gnugrep
      pkgs.hdparm
      pkgs.iozone
    ];
    text = (builtins.readFile ./test-sd-card.sh);
  };
in
{
  options = {
    hardware.az-raspi4-base.enable = lib.mkEnableOption "Enable the base config for a Raspberry Pi 4.";
  };

  config = lib.mkIf cfg.enable {
    boot.supportedFilesystems = lib.mkForce [
      "f2fs"
      "ntfs"
      "cifs"
      "ext4"
      "vfat"
      "nfs"
      "nfs4"
    ];
    boot.kernelParams = [
      "rootflags=atgc"
      "rw"
    ];

    fileSystems."/" = {
      device = "/dev/disk/by-label/root";
      fsType = "f2fs";
      options = [
        "atgc,gc_merge"
        "compress_algorithm=lz4"
        "compress_extension=*"
        "compress_chksum"
        "discard"
        "lazytime"
      ];
    };
    fileSystems."/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
    };
    swapDevices = [
      {
        device = "/var/lib/swapfile";
        size = 4 * 1024;
      }
    ];

    boot = {
      initrd.availableKernelModules = [
        "cryptd"
        "genet" # required for the ethernet to work at initrd
        "usbhid"
        "usb_storage"
        "vc4"
        "pcie_brcmstb" # required for the pcie bus to work
        "reset-raspberrypi" # required for vl805 firmware to load
      ];

      initrd.luks.devices."cryptsd" = {
        device = "/dev/disk/by-label/cryptroot";
        allowDiscards = true; # required for TRIM
      };
      loader = {
        systemd-boot.enable = true;
      };
    };
    boot.blacklistedKernelModules = [
      "brcmfmac" # diable the wifi driver
      "hci_uart" # disable the bluetooth driver
      "btbcm" # disable the bluetooth driver
      "btintel" # disable the bluetooth driver
      "btqca" # disable the bluetooth driver
      "btsdio" # disable the bluetooth driver
      "bluetooth" # disable the bluetooth driver
    ];
    boot.extraModulePackages = [ ];

    hardware.enableRedistributableFirmware = true;
    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [
      libraspberrypi
      raspberrypi-eeprom
      test-sd-card
    ];
    environment.shellAliases = {
      raspi-firmware-update = ''
        sudo mkdir -p /mnt/firmware && \
        sudo mount /dev/disk/by-label/FIRMWARE /mnt/firmware && \
        BOOTFS=/mnt/firmware FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
        sudo umount /mnt/firmware
      '';
    };
  };
}

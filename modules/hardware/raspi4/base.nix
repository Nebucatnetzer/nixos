{ inputs, lib, pkgs, ... }:
{
  boot = {
    supportedFilesystems = lib.mkForce [ "f2fs" "ntfs" "cifs" "ext4" "vfat" "nfs" "nfs4" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  boot = {
    initrd.availableKernelModules = [
      "usbhid"
      "usb_storage"
      "vc4"
      "pcie_brcmstb" # required for the pcie bus to work
      "reset-raspberrypi" # required for vl805 firmware to load
    ];

    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };
  boot.extraModulePackages = [ ];
  boot.kernelParams = [ ];

  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  environment.shellAliases = {
    raspi-cpu = ''
      sudo vcgencmd get_throttled && sudo vcgencmd measure_temp
    '';
    raspi-firmware-update = ''
      sudo mkdir -p /mnt && \
      sudo mount /dev/disk/by-label/FIRMWARE /mnt && \
      BOOTFS=/mnt FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
      sudo umount /mnt
    '';
  };
}

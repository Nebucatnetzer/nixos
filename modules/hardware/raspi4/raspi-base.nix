{ config, lib, pkgs, ... }:
let
  cfg = config.hardware.az-raspi4-base;
  test-sd-card = pkgs.writeShellScriptBin "test-sd-card" ''
    # Raspberry Pi microSD card benchmark script.
    #
    # A script I use to automate the running and reporting of benchmarks I compile
    # for: http://www.pidramble.com/wiki/benchmarks/microsd-cards
    #
    # Usage:
    #   # Run it locally.
    #   $ su do ./microsd-benchmarks.sh
    #
    #   # Run it straight from GitHub.
    #   $ curl https://raw.githubusercontent.com/geerlingguy/raspberry-pi-dramble/master/setup/benchmarks/microsd-benchmarks.sh | sudo bash
    #
    # Another good benchmark:
    #   $ curl http://www.nmacleod.com/public/sdbench.sh | sudo bash
    #
    # Author: Jeff Geerling, 2016 (last updated 2020)

    printf "\n"
    printf "Raspberry Pi Dramble microSD benchmarks\n"

    CLOCK="$(${pkgs.gnugrep}/bin/grep "actual clock" /sys/kernel/debug/mmc0/ios 2>/dev/null | ${pkgs.gawk}/bin/awk '{printf("%0.3f MHz", $3/1000000)}')"
    if [ -n "$CLOCK" ]; then
        echo "microSD clock: $CLOCK"
    fi
    printf "\n"

    # Fail if $SUDO_USER is empty.
    if [ -z "$SUDO_USER" ]; then
        printf "This script must be run with sudo.\n"
        exit 1
    fi

    # Variables.
    USER_HOME_PATH=$(getent passwd $SUDO_USER | cut -d: -f6)

    # Run benchmarks.
    printf "Running hdparm test...\n"
    ${pkgs.hdparm}/bin/hdparm -t /dev/mmcblk1
    printf "\n"

    printf "Running dd test...\n\n"
    ${pkgs.coreutils}/bin/dd if=/dev/zero of=$USER_HOME_PATH/test bs=8k count=50k conv=fsync
    rm -f $USER_HOME_PATH/test
    printf "\n"

    printf "Running iozone test...\n"
    ${pkgs.iozone}/bin/iozone -e -I -a -s 100M -r 4k -i 0 -i 1 -i 2
    printf "\n"

    printf "microSD card benchmark complete!\n\n"
  '';
in {
  options = {
    hardware.az-raspi4-base.enable =
      lib.mkEnableOption "Enable the base config for a Raspberry Pi 4.";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      supportedFilesystems =
        lib.mkForce [ "f2fs" "ntfs" "cifs" "ext4" "vfat" "nfs" "nfs4" ];
    };
    fileSystems."/" = {
      device = "/dev/disk/by-label/NixosSd";
      fsType = "ext4";
      options = [ "noatime" ];
    };
    fileSystems."/boot" = {
      device = "/dev/disk/by-label/SdBoot";
      fsType = "vfat";
    };

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

      initrd.luks.devices."cryptlvmsd".device = "/dev/mmcblk1p2";
      initrd.network = {
        enable = true;
        ssh = {
          enable = true;
          port = 22;
          shell = "/bin/cryptsetup-askpass";
          authorizedKeys =
            config.users.users.${config.az-username}.openssh.authorizedKeys.keys;
          hostKeys = [
            "/etc/secrets/initrd/ssh_host_rsa_key"
            "/etc/secrets/initrd/ssh_host_ed25519_key"
          ];
        };
      };
      loader = { systemd-boot.enable = true; };
    };
    boot.extraModulePackages = [ ];

    hardware.enableRedistributableFirmware = true;
    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [
      libraspberrypi
      raspberrypi-eeprom
      test-sd-card
    ];
    environment.shellAliases = {
      raspi-cpu = ''
        sudo vcgencmd get_throttled && sudo vcgencmd measure_temp
      '';
      raspi-firmware-update = ''
        sudo mkdir -p /mnt/firmware && \
        sudo mount /dev/disk/by-label/FIRMWARE /mnt/firmware && \
        BOOTFS=/mnt/firmware FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
        sudo umount /mnt/firmware
      '';
    };
  };
}

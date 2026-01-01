{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  btrfsModule = import "${inputs.self}/modules/hardware/btrfs";
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
  imports = [
    "${inputs.self}/modules/services/log-to-ram"
    "${inputs.self}/modules/services/zram-swap"
    (btrfsModule { })
  ];
  boot.supportedFilesystems.zfs = lib.mkForce false;
  boot.kernelParams = [
    "rootflags=atgc"
    "rw"
  ];

  zramSwap.algorithm = "lz4";

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
  services.pulseaudio.enable = true;

  environment.systemPackages = [
    pkgs.libraspberrypi
    pkgs.raspberrypi-eeprom
    test-sd-card
  ];
  nixpkgs.hostPlatform = "aarch64-linux";
}

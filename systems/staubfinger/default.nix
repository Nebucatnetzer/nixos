{ inputs, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    "${inputs.self}/hardware/bluetooth"
    "${inputs.self}/modules/desktop"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/droidcam"
    "${inputs.self}/modules/espanso"
    "${inputs.self}/modules/lockscreen"
    "${inputs.self}/modules/restic"
    "${inputs.self}/modules/tlp"
  ];
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cryptd"
    "sd_mod"
    "usb_storage"
    "xhci_pci"
  ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "acpi_osi="
  ];
  boot.initrd.luks.devices."cryptlvm".device = "/dev/sda2";

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  networking.hostName = "staubfinger";

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  virtualisation.virtualbox.host.enable = true;
}


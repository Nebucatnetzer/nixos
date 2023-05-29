{ hostname }: { inputs, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    "${inputs.self}/modules/desktop"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/restic-client-desktop"
    "${inputs.self}/modules/tlp"
    "${inputs.self}/modules/tmux"
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

  networking.hostName = hostname;

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  hardware = {
    az-bluetooth.enable = true;
    az-dvd.enable = true;
  };

  programs = {
    az-lockscreen.enable = true;
    az-makemkv.enable = true;
  };
  services.az-x86.enable = true;

  virtualisation.virtualbox.host.enable = true;
}


{ hostname }: { inputs, lib, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    "${inputs.self}/modules/logs-share"
    "${inputs.self}/modules/restic-client-desktop"
    "${inputs.self}/modules/tlp"
  ];
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
  boot.kernelModules = [ "kvm-intel" "sg" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [ ];
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  boot.initrd.luks.devices."cryptlvm".device = "/dev/nvme0n1p2";

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  hardware.video.hidpi.enable = true;

  networking.hostName = hostname;

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  hardware = {
    az-bluetooth.enable = true;
    az-dvd.enable = true;
    az-nvidia.enable = true;
  };

  profiles.az-desktop.enable = true;
  programs = {
    az-lockscreen.enable = true;
    az-makemkv.enable = true;
  };

  services = {
    az-rdp.enable = true;
    az-x86.enable = true;
  };

  virtualisation.virtualbox.host.enable = true;
}


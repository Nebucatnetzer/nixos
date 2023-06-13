{ hostname }: { inputs, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
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
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/5d2e0ae2-8604-4921-b8b7-731358220a0f";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/ED86-8844";
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
  profiles.az-desktop.enable = true;
  programs = {
    az-lockscreen.enable = true;
    az-makemkv.enable = true;
    az-restic-management.enable = true;
    az-tmux.enable = true;
  };
  services = {
    az-docker.enable = true;
    az-restic-client-desktop.enable = true;
    az-tlp.enable = true;
    az-x86.enable = true;
  };
  virtualisation.virtualbox.host.enable = true;
}


{ hostname }:
{ inputs, ... }:
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
  boot.initrd.luks.devices."cryptlvm" = {
    allowDiscards = true;
    device = "/dev/sda2";
  };
  boot.initrd.luks.devices."cryptswap" = {
    allowDiscards = true;
    device = "/dev/sda3";
  };
  boot.kernelParams = [
    "acpi_osi=" # required for hardware support
    "ip=dhcp" # required for ssh at initrd
  ];

  boot.loader.efi.efiSysMountPoint = "/boot/efi";
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
    device = "/dev/disk/by-uuid/5d2e0ae2-8604-4921-b8b7-731358220a0f";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/ED86-8844";
    fsType = "vfat";
  };

  networking.hostName = hostname;

  swapDevices = [ { device = "/dev/disk/by-uuid/17a8bd01-095b-41ae-8b90-ecc70ab7b7eb"; } ];

  hardware = {
    az-bluetooth.enable = true;
    az-dvd.enable = true;
  };
  profiles.az-desktop.enable = true;
  programs = {
    az-lockscreen.enable = true;
    az-makemkv.enable = true;
    az-restic-management.enable = true;
  };
  services = {
    az-docker.enable = true;
    az-restic-client-desktop.enable = true;
    az-tlp.enable = true;
    az-x86.enable = true;
    fstrim.enable = true; # Enable TRIM for SD cards
  };
  virtualisation.virtualbox.host.enable = true;
}

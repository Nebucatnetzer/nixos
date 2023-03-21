{ custom, hostname }: { ... }:
{
  imports = [
    custom.inputs.nixos-hardware.nixosModules.common-gpu-intel
    custom.inputs.nixos-hardware.nixosModules.common-pc-laptop
    custom.inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    "${custom.inputs.self}/hardware/bluetooth"
    (import "${custom.inputs.self}/modules/desktop" { inherit custom; })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/restic-client-desktop" { inherit custom; })
    "${custom.inputs.self}/modules/tmux"
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

  virtualisation.virtualbox.host.enable = true;
}


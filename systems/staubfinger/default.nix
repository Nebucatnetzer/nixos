{ custom, hostname, inputs }: { ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    "${inputs.self}/hardware/bluetooth"
    (import "${inputs.self}/modules/desktop" { inherit custom inputs; })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    (import "${inputs.self}/modules/droidcam" { inherit custom; })
    (import "${inputs.self}/modules/espanso" { inherit custom; })
    "${inputs.self}/modules/lockscreen"
    (import "${inputs.self}/modules/restic" { inherit custom inputs; })
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

  virtualisation.virtualbox.host.enable = true;
}


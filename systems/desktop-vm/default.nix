{ hostname, inputs, ... }:
{
  imports = [
    "${inputs.self}/modules/desktop"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/espanso"
    "${inputs.self}/modules/logs-share"
    "${inputs.self}/modules/nix-direnv"
    "${inputs.self}/modules/tmux"
  ];
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "ohci_pci"
    "sd_mod"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  networking = {
    hostName = hostname;
    interfaces.enp0s3.useDHCP = true;
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  virtualisation.virtualbox.guest.enable = true;
}


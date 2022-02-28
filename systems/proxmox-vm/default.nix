{ inputs, hostname, ip, ... }:
{
  imports = [
    (import "${inputs.self}/modules/mk-network" { inherit hostname ip; })
  ];
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "uhci_hcd"
    "virtio_pci"
    "virtio_scsi"
    "sd_mod"
    "sr_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "dm-snapshot" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  # Inspired by
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/minimal.nix
  environment.noXlibs = true;
  documentation.enable = false;
  documentation.nixos.enable = false;
  programs.command-not-found.enable = false;
}


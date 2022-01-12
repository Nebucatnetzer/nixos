{ ... }:
{
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
  fileSystems."/mnt/data" = {
    device = "10.7.89.108:raspi_data";
    fsType = "nfs";
  };

  networking = {
    hostName = "heimdall";
    hosts = {
      "127.0.0.1" = [ "heimdall.2li.local" ];
      "10.7.89.121" = [ "heimdall.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.2" ];
    interfaces.ens18.ipv4.addresses = [
      {
        address = "10.7.89.121";
        prefixLength = 24;
      }
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];
}


{ custom, hostname }: { lib, ... }:
{
  imports = [
    custom.inputs.nixos-hardware.nixosModules.dell-precision-5530
    custom.inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    custom.inputs.nixos-hardware.nixosModules.common-gpu-intel
    (import "${custom.inputs.self}/modules/desktop" { inherit custom; })
    "${custom.inputs.self}/modules/logs-share"
    (import "${custom.inputs.self}/modules/restic-client-desktop" { inherit custom; })
    "${custom.inputs.self}/modules/tlp"
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
    az_bluetooth.enable = true;
    az_nvidia.enable = true;
    dvd.enable = true;
  };

  programs = {
    lockscreen.enable = true;
    makemkv.enable = true;
  };

  services.rdp.enable = true;

  virtualisation.virtualbox.host.enable = true;
}


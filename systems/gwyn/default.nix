{ custom, hostname }: { lib, ... }:
{
  imports = [
    custom.inputs.nixos-hardware.nixosModules.dell-precision-5530
    custom.inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    custom.inputs.nixos-hardware.nixosModules.common-gpu-intel
    "${custom.inputs.self}/hardware/bluetooth"
    "${custom.inputs.self}/hardware/nvidia"
    "${custom.inputs.self}/hardware/dvd"
    "${custom.inputs.self}/modules/data-share"
    (import "${custom.inputs.self}/modules/desktop" { inherit custom; })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/droidcam" { inherit custom; })
    (import "${custom.inputs.self}/modules/email" { inherit custom; })
    (import "${custom.inputs.self}/modules/eog" { inherit custom; })
    (import "${custom.inputs.self}/modules/espanso" { inherit custom; })
    "${custom.inputs.self}/modules/lockscreen"
    "${custom.inputs.self}/modules/logs-share"
    (import "${custom.inputs.self}/modules/nix-direnv" { inherit custom; })
    (import "${custom.inputs.self}/modules/pipewire" { inherit custom; })
    "${custom.inputs.self}/modules/scripts"
    "${custom.inputs.self}/modules/tlp"
    "${custom.inputs.self}/modules/tmux"
    (import "${custom.inputs.self}/modules/restic" { inherit custom; })
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
  boot.kernelModules = [ "kvm-intel" ];
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

  networking.hostName = hostname;

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  virtualisation.virtualbox.host.enable = true;
}


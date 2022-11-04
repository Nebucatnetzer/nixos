{ custom, hostname, inputs }: { lib, ... }:
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    "${inputs.self}/hardware/bluetooth"
    "${inputs.self}/hardware/nvidia"
    "${inputs.self}/hardware/dvd"
    "${inputs.self}/modules/data-share"
    (import "${inputs.self}/modules/desktop" { inherit custom inputs; })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    (import "${inputs.self}/modules/droidcam" { inherit custom; })
    (import "${inputs.self}/modules/eog" { inherit custom; })
    (import "${inputs.self}/modules/espanso" { inherit custom; })
    "${inputs.self}/modules/lockscreen"
    "${inputs.self}/modules/logs-share"
    (import "${inputs.self}/modules/nix-direnv" { inherit custom; })
    (import "${inputs.self}/modules/pipewire" { inherit custom; })
    "${inputs.self}/modules/scripts"
    "${inputs.self}/modules/tlp"
    "${inputs.self}/modules/tmux"
    (import "${inputs.self}/modules/restic" { inherit custom hostname inputs; })
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


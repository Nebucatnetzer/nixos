{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [ inputs.nixos-hardware.nixosModules.dell-precision-5530 ];

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
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "ip=dhcp" # required for ssh at initrd
  ];

  boot.initrd.luks.devices."cryptlvm" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
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
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  networking.hostName = hostname;

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  hardware = {
    az-bluetooth.enable = true;
    az-dvd.enable = true;
    az-nvidia.enable = true;
    opengl.enable = true;
  };

  nix.settings = {
    substituters = [
      "ssh://nix-ssh@management.2li.local?priority=50"
    ];
    secret-key-files = config.age.secrets.signingKey.path;
  };

  age.secrets.gwynRootSshKey = {
    file = "${inputs.self}/scrts/gwyn_root_ssh_key.age";
    path = "/root/.ssh/id_ed25519";
    mode = "600";
    owner = "root";
    group = "root";
  };

  age.secrets.signingKey = {
    file = "${inputs.self}/scrts/signing.key.age";
    mode = "600";
    owner = "root";
    group = "root";
  };

  profiles.az-desktop.enable = true;
  programs = {
    az-adb.enable = true;
    az-distrobox.enable = true;
    az-lockscreen.enable = true;
    az-makemkv.enable = true;
    az-restic-management.enable = true;
  };

  services = {
    az-logs-share.enable = true;
    az-qtile.enable = true;
    az-restic-client-desktop.enable = true;
    az-tlp.enable = true;
    az-x86.enable = true;
    az-zram-swap.enable = true;
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    logind.lidSwitchExternalPower = "ignore";

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

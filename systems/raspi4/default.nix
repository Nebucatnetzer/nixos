{ inputs, hostname, ip, pkgs, ... }:
{
  imports = [
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS";
      fsType = "ext4";
      options = [
        "noatime"
      ];
    };
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  boot.initrd.availableKernelModules = [
    "reset_raspberrypi"
    "sdhci_pci"
    "ahci"

    "ata_piix"

    "sata_inic162x"
    "sata_nv"
    "sata_promise"
    "sata_qstor"
    "sata_sil"
    "sata_sil24"
    "sata_sis"
    "sata_svw"
    "sata_sx4"
    "sata_uli"
    "sata_via"
    "sata_vsc"

    "pata_ali"
    "pata_amd"
    "pata_artop"
    "pata_atiixp"
    "pata_efar"
    "pata_hpt366"
    "pata_hpt37x"
    "pata_hpt3x2n"
    "pata_hpt3x3"
    "pata_it8213"
    "pata_it821x"
    "pata_jmicron"
    "pata_marvell"
    "pata_mpiix"
    "pata_netcell"
    "pata_ns87410"
    "pata_oldpiix"
    "pata_pcmcia"
    "pata_pdc2027x"
    "pata_qdi"
    "pata_rz1000"
    "pata_serverworks"
    "pata_sil680"
    "pata_sis"
    "pata_sl82c105"
    "pata_triflex"
    "pata_via"
    "pata_winbond"

    # SCSI support (incomplete).
    "3w-9xxx"
    "3w-xxxx"
    "aic79xx"
    "aic7xxx"
    "arcmsr"

    # USB support, especially for booting from USB CD-ROM
    # drives.
    "uas"

    # SD cards.
    "sdhci_pci"

    # Allows using framebuffer configured by the initial boot firmware
    "simplefb"

    # Broadcom

    "vc4"

    # Broadcom

    "pcie-brcmstb"

    # Misc. uncategorized hardware

    # Used for some platform's integrated displays
    "panel-simple"
    "pwm-bl"

    # Power supply drivers, some platforms need them for USB
    "axp20x-ac-power"
    "axp20x-battery"
    "pinctrl-axp209"
    "mp8859"

    # USB drivers
    "xhci-pci-renesas"

    # Misc "weak" dependencies
    "analogix-dp"
    "analogix-anx6345" # For DP or eDP (e.g. integrated display)
  ];
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.grub.enable = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.device = "nodev";
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    raspberrypi-eeprom
  ];

  networking = {
    useDHCP = false;
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.2" ];
    interfaces.eth0.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
  };

  environment.shellAliases = {
    raspi-firmware-update = ''
      sudo mount /dev/disk/by-label/FIRMWARE /mnt && \
      BOOTFS=/mnt FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
      sudo umount /mnt
    '';
  };
}

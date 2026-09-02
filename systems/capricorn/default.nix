{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  btrfsLayout = import "${inputs.self}/modules/hardware/btrfs/layout.nix";
  foxFlss = inputs.fox-flss.packages.${pkgs.stdenv.hostPlatform.system}.default;
  foxFlssWrapper = pkgs.writeShellApplication {
    name = "enable-wwan";
    runtimeInputs = [
      pkgs.networkmanager
    ];
    text = ''
      if [ "$EUID" -ne 0 ]; then
        echo "Please run as root"
        exit
      fi
      mkdir -p /var/log/FoxFlss/Log/
      ${foxFlss}/usr/bin/FoxFlss "$@"
      nmcli radio wwan on
      nmcli connection up yallo
    '';
  };
  nixBuilderModule = import "${inputs.self}/modules/services/nix-remote-builder";
  resticClientModule = import "${inputs.self}/modules/services/restic-client";
  syncthingModule = import "${inputs.self}/modules/services/syncthing";
  wireguardClient = import "${inputs.self}/modules/services/wireguard/client.nix";
in
{
  imports = [
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/hardware/dvd"
    "${inputs.self}/modules/profiles/desktop"
    "${inputs.self}/modules/programs/adb"
    "${inputs.self}/modules/programs/distrobox"
    "${inputs.self}/modules/programs/droidcam"
    #     "${inputs.self}/modules/programs/makemkv" (temporarily disabled: upstream download 525)
    "${inputs.self}/modules/services/kanata"
    "${inputs.self}/modules/services/kde"
    "${inputs.self}/modules/services/zram-swap"
    (btrfsAuxModule { })
    (btrfsLayout { })
    (nixBuilderModule { role = "client"; })
    (resticClientModule { })
    (syncthingModule { })
    (wireguardClient {
      IP = config.az-hosts."${hostname}".wgIp;
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
    })
  ];

  age.secrets.wireguardPrivateKey.file = "${inputs.self}/scrts/capricorn_wg.key.age";

  # Capricorn is a Dell Latitude 7450 with an Intel Core Ultra 7 165U CPU of generation Meteor Lake.
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cryptd"
    "nvme"
    "rtsx_pci_sdmmc"
    "sd_mod"
    "sr_mod"
    "thunderbolt"
    "usbhid"
    "usb_storage"
    "xhci_pci"
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.kernelModules = [
    "i915" # graphics driver (xe was unstable: VCS media engine job timeouts under HW video decode)
    "dm-snapshot"
    "thunderbolt"
  ];
  boot.kernelModules = [
    "squashfs"
    "v4l2loopback"
  ];
  boot.extraModprobeConfig = ''options v4l2loopback exclusive_caps=1 card_label="Intel MIPI Camera"'';
  boot.extraModulePackages = [
    config.boot.kernelPackages.v4l2loopback
  ];
  boot.resumeDevice = "/dev/mapper/mainLuks";
  boot.kernelParams = [
    "i915.force_probe=7d45"
    "xe.force_probe=!7d45"
    # Hibernation: swapfile on the btrfs inside mainLuks.
    # resume_offset comes from `btrfs inspect-internal map-swapfile -r /swap/swapfile`
    # and must be regenerated if the swapfile is ever recreated or moved.
    "resume_offset=533760"
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  networking.wg-quick.interfaces.wg0.dns = [ config.az-hosts.gwyn.wgIp ];
  networking.hostName = hostname;

  hardware = {
    cpu.intel.npu.enable = true;
    graphics = {
      extraPackages = [
        pkgs.intel-compute-runtime
        pkgs.intel-media-driver # For Broadwell (2014) or newer processors. LIBVA_DRIVER_NAME=iHD
        pkgs.intel-ocl
        pkgs.libvdpau-va-gl
        pkgs.vpl-gpu-rt
      ];
    };
    keyboard.zsa.enable = true;
    ipu6 = {
      enable = true;
      platform = "ipu6epmtl";
    };
  };

  environment.systemPackages = [
    foxFlssWrapper
  ];
  programs = {
    kdeconnect.enable = true;
  };

  services.logind.settings.Login.HandleLidSwitch = "suspend-then-hibernate";
  systemd.sleep.settings.Sleep.HibernateMode = "shutdown";
  systemd.sleep.settings.Sleep.HibernateDelaySec = "30min";
  # The LPSS I²C controller carrying the touchpad does not survive S4 and cannot
  # be revived by rebinding, so unbind it before the snapshot and re-probe on resume.
  powerManagement.powerDownCommands = ''
    echo 0000:00:15.3 > /sys/bus/pci/drivers/intel-lpss/unbind
  '';
  powerManagement.resumeCommands = ''
    echo 0000:00:15.3 > /sys/bus/pci/drivers/intel-lpss/bind
  '';

  services = {
    fprintd.enable = true;
    smartd.devices = [
      { device = "/dev/nvme0n1"; }
    ];
  };
}

{ hostname }:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  actualBudgetModule = import "${inputs.self}/modules/services/actualbudget";
  actualData = "/var/lib/actualbudget";
  eactualData = "/var/lib/eactual";
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
  domains = [
    { fqdn = "${config.services.freshrss.virtualHost}"; }
    { fqdn = "${giteaDomain}"; }
    { fqdn = rssBridgeDomain; }
    { fqdn = "www.zweili.ch"; }
    { fqdn = "search.zweili.org"; }
    { fqdn = "searxng.zweili.org"; }
  ];
  giteaDomain = "git.zweili.org";
  giteaModule = import "${inputs.self}/modules/services/gitea";
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate";
  mediaShare = import "${inputs.self}/modules/services/media-share";
  resticClientModule = import "${inputs.self}/modules/services/restic-client";
  rssBridgeDomain = "rss-bridge.zweili.org";
  rssBridgeModule = import "${inputs.self}/modules/services/rss-bridge";
  syncthingModule = import "${inputs.self}/modules/services/syncthing";
  wireguardModule = import "${inputs.self}/modules/services/wireguard";
in
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/misc/initrd-ssh"
    "${inputs.self}/modules/profiles/management"
    "${inputs.self}/modules/services/blog"
    "${inputs.self}/modules/services/dnsmasq"
    "${inputs.self}/modules/services/davis"
    "${inputs.self}/modules/services/freshrss"
    "${inputs.self}/modules/services/haproxy"
    "${inputs.self}/modules/services/librenms"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/plex"
    "${inputs.self}/modules/services/search"
    "${inputs.self}/modules/services/snmpd"
    "${inputs.self}/modules/services/syslog"
    "${inputs.self}/modules/services/wireguard/routing.nix"
    "${inputs.self}/modules/services/zram-swap"
    (actualBudgetModule {
      domain = "actual.zweili.org";
      dataDirectory = actualData;
      name = "actual";
      port = 5006;
    })
    (actualBudgetModule {
      domain = "eactual.zweili.org";
      dataDirectory = eactualData;
      name = "eactual";
      port = 5007;
    })
    (btrfsAuxModule { })
    (mediaShare { hard = true; })
    (giteaModule { domain = giteaDomain; })
    (librenmsCertificateModule { inherit domains; })
    (resticClientModule {
      paths = [
        actualData
        eactualData
        "/var/lib/plex"
        config.services.freshrss.dataDir
      ];
      mariadb = true;
      resticSchedule = "*-*-* 06..21:30:00";
    })
    (rssBridgeModule {
      domain = rssBridgeDomain;
    })
    (syncthingModule { exposeWebInterface = true; })
    (wireguardModule {
      IP = config.az-hosts."${hostname}".wgIp;
      isHub = true;
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
    })
  ];

  age.secrets.wireguardPrivateKey.file = "${inputs.self}/scrts/gwyn_wg.key.age";
  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "ahci"
    "cdc_ether"
    "cryptd"
    "nvme"
    "rtsx_pci_sdmmc"
    "r8152"
    "r8153_ecm"
    "sd_mod"
    "sr_mod"
    "usbhid"
    "usb_storage"
    "xhci_pci"
  ];

  boot.initrd.kernelModules = [
    "cdc_ether"
    "dm-snapshot"
    "r8152"
    "r8153_ecm"
    "xhci_pci"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "rootdelay=10"
    "ip=dhcp" # required for ssh at initrd
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  boot.supportedFilesystems = [
    "btrfs"
    "cifs"
    "exfat"
    "ext4"
    "f2fs"
    "nfs"
    "nfs4"
    "ntfs"
    "squashfs"
  ];

  fileSystems."/" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=root"
    ]
    ++ commonBtrfsOptions;
  };
  fileSystems."/home" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=home"
    ]
    ++ commonBtrfsOptions;
  };
  fileSystems."/nix" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    neededForBoot = true;
    options = [
      "subvol=nix"
    ]
    ++ commonBtrfsOptions;
  };
  fileSystems."/swap" = {
    fsType = "btrfs";
    label = "mainBtrfs";
    options = [
      "compress=no"
      "noatime"
      "nodatacow"
      "nodatasum"
      "ssd"
      "subvol=swap"
    ];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  swapDevices = [ { device = "/swap/swapfile"; } ];

  # USB address of the ethernet dongle: 0bda:8153
  networking = {
    useDHCP = false;
    hostName = hostname;
    defaultGateway = config.az-hosts.loki.physicalIp;
    nameservers = [ config.az-hosts.loki.physicalIp ];
    interfaces.enp58s0u1.ipv4.addresses = [
      {
        address = config.az-hosts."${hostname}".physicalIp;
        prefixLength = 24;
      }
    ];
  };

  hardware.graphics.enable = true;
  hardware.nvidia-container-toolkit.enable = true;

  services = {
    fstrim.enable = true; # Enable TRIM for SD cards
    hardware.bolt.enable = true; # Enable Thunderbolt control
    logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
    mysql.package = pkgs.mariadb_114;
    nginx.defaultHTTPListenPort = 8080;
    nginx.defaultListenAddresses = [ "127.0.0.1" ];
    nginx.defaultSSLListenPort = 8443;
    thermald.enable = true;

    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

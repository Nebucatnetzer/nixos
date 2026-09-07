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
  archiveLuks = "archiveLuks";
  archivePath = "/mnt/archive-disk";
  archiveShareServer = import "${inputs.self}/modules/services/archive-share/server.nix";
  eactualData = "/var/lib/eactual";
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  btrfsLayout = import "${inputs.self}/modules/hardware/btrfs/layout.nix";
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
  domains = [
    { fqdn = "${config.services.freshrss.virtualHost}"; }
    { fqdn = "${giteaDomain}"; }
    { fqdn = rssBridgeDomain; }
    { fqdn = "www.zweili.ch"; }
    { fqdn = "search.zweili.org"; }
    { fqdn = "searxng.zweili.org"; }
  ];
  giteaDataDir = "/var/lib/gitea";
  giteaDomain = "git.zweili.org";
  giteaModule = import "${inputs.self}/modules/services/gitea";
  hddBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/hdd_options.nix";
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate";
  nixBuilderModule = import "${inputs.self}/modules/services/nix-remote-builder";
  resticClientModule = import "${inputs.self}/modules/services/restic-client";
  resticServer = import "${inputs.self}/modules/services/restic-server";
  rssBridgeDomain = "rss-bridge.zweili.org";
  rssBridgeModule = import "${inputs.self}/modules/services/rss-bridge";
  syncthingModule = import "${inputs.self}/modules/services/syncthing";
  wireguardHub = import "${inputs.self}/modules/services/wireguard/hub.nix";
in
{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-precision-5530
    "${inputs.self}/modules/hardware/bluetooth"
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/misc/initrd-ssh"
    "${inputs.self}/modules/profiles/management"
    "${inputs.self}/modules/services/blog"
    "${inputs.self}/modules/services/ddclient"
    "${inputs.self}/modules/services/pihole"
    "${inputs.self}/modules/services/freshrss"
    "${inputs.self}/modules/services/librenms"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/ntp"
    "${inputs.self}/modules/services/search"
    "${inputs.self}/modules/services/snmpd"
    "${inputs.self}/modules/services/syslog"
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
    (archiveShareServer {
      clients = [ config.az-hosts.capricorn.wgIp ];
      path = archivePath;
    })
    (btrfsAuxModule {
      mountPaths = [
        "/"
        archivePath
      ];
    })
    (btrfsLayout { })
    (nixBuilderModule { role = "client"; })
    (giteaModule {
      dataDir = giteaDataDir;
      domain = giteaDomain;
    })
    (librenmsCertificateModule { inherit domains; })
    (resticClientModule {
      paths = [
        actualData
        eactualData
        giteaDataDir
        config.services.freshrss.dataDir
        config.services.librenms.dataDir
      ];
      mariadb = true;
      resticSchedule = "*-*-* 00..06,09..23:45:00";
    })
    (resticServer { })
    (rssBridgeModule {
      domain = rssBridgeDomain;
    })
    (syncthingModule { exposeWebInterface = true; })
    (wireguardHub {
      IP = config.az-hosts."${hostname}".wgIp;
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
    })
  ];

  age.secrets.archiveLuksKey = {
    file = "${inputs.self}/scrts/gwyn_archive_luks.key.age";
    mode = "400";
  };
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
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "rootdelay=10"
    "ip=${
      config.az-hosts."${hostname}".physicalIp
    }::${config.az-hosts.loki.physicalIp}:255.255.255.0:${hostname}:enp58s0u1" # required for ssh at initrd
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/nvme0n1p2";
  };

  # Stage 2, not initrd: the root disk is unlocked interactively over initrd ssh, and a
  # second prompt there would also mean a missing USB disk stalls the boot of a headless
  # host. nixpkgs has no option for a non-root LUKS device, so this is the crypttab that
  # systemd-cryptsetup-generator reads. The keyfile lives on the encrypted root.
  environment.etc.crypttab.text = ''
    ${archiveLuks} UUID=cffc97f9-48e6-48b2-9d90-876f7775a684 ${config.age.secrets.archiveLuksKey.path} luks,nofail,x-systemd.device-timeout=10
  '';
  # nofail keeps a missing archive disk from blocking boot, which is the whole reason the
  # unlock moved out of initrd. The other half of that trade is that an absent disk leaves
  # this path an empty directory on the root filesystem, so anything reading the archive
  # has to check it is a mountpoint rather than trusting the path to exist.
  fileSystems."${archivePath}" = {
    device = "/dev/mapper/${archiveLuks}";
    fsType = "btrfs";
    neededForBoot = false;
    options = [
      "nofail"
      "x-systemd.device-timeout=10"
    ]
    ++ hddBtrfsOptions;
  };

  # nofail keeps a missing repo disk from blocking boot; the restic-server module pairs
  # it with RequiresMountsFor so the services fail instead of writing to the root disk.
  fileSystems."/var/lib/restic-server" = {
    fsType = "btrfs";
    label = "resticSSD";
    neededForBoot = false;
    options = [
      "subvol=restic-repo"
      "nofail"
      "x-systemd.device-timeout=10"
    ]
    ++ commonBtrfsOptions;
  };

  # USB address of the ethernet dongle: 0bda:8153
  networking = {
    enableIPv6 = false;
    useDHCP = false;
    hostName = hostname;
    defaultGateway = config.az-hosts.loki.physicalIp;
    nameservers = [ "127.0.0.1" ];
    firewall.allowedTCPPorts = [
      80
      443
    ];
    interfaces.enp58s0u1.ipv4.addresses = [
      {
        address = config.az-hosts."${hostname}".physicalIp;
        prefixLength = 24;
      }
    ];
  };

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_580;

  services = {
    logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
    mysql.package = pkgs.mariadb_114;
    smartd.devices = [
      { device = "/dev/nvme0n1"; }
    ];
    # Disable the integrated webcam
    udev.extraRules = ''
      ACTION=="add", ATTR{idVendor}=="0c45", ATTR{idProduct}=="671d", RUN="${pkgs.bash}/bin/sh -c 'echo 1 >/sys/\$devpath/remove'"
    '';
  };
}

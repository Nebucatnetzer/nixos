{ hostname }:
{
  config,
  inputs,
  ...
}:
let
  btrfsAuxModule = import "${inputs.self}/modules/hardware/btrfs/aux.nix";
  btrfsLayout = import "${inputs.self}/modules/hardware/btrfs/layout.nix";
  nixBuilderModule = import "${inputs.self}/modules/services/nix-remote-builder";
  wireguardClient = import "${inputs.self}/modules/services/wireguard/client.nix";
in
{
  imports = [
    "${inputs.self}/modules/hardware/common-x86"
    "${inputs.self}/modules/misc/initrd-ssh"
    "${inputs.self}/modules/profiles/management"
    "${inputs.self}/modules/services/zram-swap"
    (btrfsAuxModule { })
    (btrfsLayout { })
    (nixBuilderModule { role = "server"; })
    (wireguardClient {
      IP = config.az-hosts."${hostname}".wgIp;
      privateKeyFile = config.age.secrets.wireguardPrivateKey.path;
      # fenoglio sits on the LAN (eno2); don't route 10.7.89.0/24 through the tunnel
      routeLan = false;
      # reach gwyn directly over the LAN instead of hairpinning via the public IP
      hubHost = config.az-hosts.gwyn.physicalIp;
    })
  ];

  age.secrets.wireguardPrivateKey.file = "${inputs.self}/scrts/fenoglio_wg.key.age";

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usb_storage"
    "sd_mod"
    "rtsx_pci_sdmmc"
    "e1000e" # onboard NIC, needed for initrd remote-unlock — CONFIRM with `ethtool -i eno2`
  ];

  # static IP in initrd so LUKS can be unlocked remotely over SSH
  boot.kernelParams = [
    "ip=${
      config.az-hosts."${hostname}".physicalIp
    }::${config.az-hosts.loki.physicalIp}:255.255.255.0:${hostname}:eno2"
  ];

  boot.initrd.luks.devices."mainLuks" = {
    allowDiscards = true;
    device = "/dev/disk/by-uuid/d3219e7e-680a-4657-aeca-0e2619575140";
  };

  networking = {
    hostName = hostname;
    useDHCP = false;
    defaultGateway = config.az-hosts.loki.physicalIp;
    nameservers = [ config.az-hosts.gwyn.physicalIp ]; # pihole runs on gwyn
    interfaces.eno2.ipv4.addresses = [
      {
        address = config.az-hosts."${hostname}".physicalIp;
        prefixLength = 24;
      }
    ];
  };

  services = {
    logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
    smartd.devices = [
      { device = "/dev/nvme0n1"; }
    ];
  };

}

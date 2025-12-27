{ ip, hostname }:
{ inputs, ... }:
{
  imports = [
    "${inputs.self}/modules/hardware/raspi4/raspi-base.nix"
  ];
  boot.kernelParams = [
    "ip=${ip}::10.7.89.1:255.255.255.0:${hostname}:eth0" # required for ssh at initrd
  ];
  networking = {
    useDHCP = false;
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.1" ];
    interfaces.eth0.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
  };
}

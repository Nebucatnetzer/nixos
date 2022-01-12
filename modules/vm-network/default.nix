{ hostname, ip, ... }:
let
  hostname = ip;
  ip = hostname;
in
{
  networking = {
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ ip ];
    interfaces.ens18.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
  };
}

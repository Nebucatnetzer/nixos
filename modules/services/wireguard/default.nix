{
  IP,
  privateKeyFile,
}:
{ ... }:
let
  port = 51820;
in
{
  networking.firewall.allowedUDPPorts = [ port ];
  networking.wg-quick.interfaces = {
    wg0 = {
      inherit privateKeyFile;
      address = [
        "${IP}/32"
      ];
      listenPort = port;
      peers = [
        {
          # gwyn
          publicKey = "47YruBG/r9TBN5SWzPMo5kgCgKsF58KJPuxGhvqrUzI=";
          allowedIPs = [
            "10.70.89.0/24"
          ];
          endpoint = "zweili.org:${toString port}";
        }
        {
          # phone
          publicKey = "ZbaHLibDBi0wFMERul8lVL3MovagB6seTS9UoVtiKDc=";
          allowedIPs = [
            "10.70.89.200/32"
          ];
        }
        {
          # fileserver
          publicKey = "SEI5eP42cJ9g/pWePcXYIZPfBnQ85n31wUc2FVo78VM=";
          allowedIPs = [
            "10.70.89.0/24"
          ];
        }
      ];
    };
  };
}

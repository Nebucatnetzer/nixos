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
          # capricorn
          publicKey = "BzZMupEJwJXzZMHDRXcLLLPsHI8HLORiy44vbekvBzU=";
          allowedIPs = [
            "10.70.89.0/24"
          ];
        }
        {
          # gwyn
          publicKey = "A96R1JeeZqT+pz15Dfl++1zmW8hpyefScQDNcxwB5l4=";
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
          publicKey = "ogbiAs4JY+tH/+M0JGN/5ZuVJ4SdFhK/pq+4LhmPmGw=";
          allowedIPs = [
            "10.70.89.108/32"
          ];
        }
      ];
    };
  };
}

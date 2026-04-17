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
            "10.70.89.170/32"
          ];
        }
        {
          # gwyn
          publicKey = "A96R1JeeZqT+pz15Dfl++1zmW8hpyefScQDNcxwB5l4=";
          allowedIPs = [
            "10.7.89.0/24"
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
          publicKey = "MGmlmY7BnJrFLOyF8CzXAL1p7/mOe9Y2EYqGyRGHSSk=";
          allowedIPs = [
            "10.70.89.180/32"
          ];
        }
      ];
    };
  };
}

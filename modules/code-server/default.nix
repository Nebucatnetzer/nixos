{ ... }:
let
  username = import ../../username.nix;
in
{
  services.code-server = {
    enable = true;
    user = username;
    host = "0.0.0.0";
    auth = "none";
  };
  networking.firewall.allowedTCPPorts = [ 4444 ];
}

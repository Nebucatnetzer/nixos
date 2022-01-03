{ ... }:
let
  username = import ../../username.nix;
in
{
  services.code-server = {
    enable = true;
    user = username;
  };
  networking.firewall.allowedTCPPorts = [ 4444 ];
}

{ pkgs, ... }:
{
  services.xrdp = {
    enable = true;
  };
  networking.firewall.allowedTCPPorts = [ 3389 ];
}

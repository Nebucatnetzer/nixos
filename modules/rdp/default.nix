{ pkgs, ... }:
{
  services.xrdp = {
    enable = true;
    defaultWindowManager = "${pkgs.qtile}/bin/qtile start";
  };
  networking.firewall.allowedTCPPorts = [ 3389 ];
}

{ config, lib, pkgs, ... }:
let
  cfg = config.services.rdp;
in
{
  options = {
    services.rdp.enable = lib.mkEnableOption "enable rdp";
  };

  config = lib.mkIf cfg.enable {
    services.xrdp = {
      enable = true;
      defaultWindowManager = "${pkgs.qtile}/bin/qtile start";
    };
    networking.firewall.allowedTCPPorts = [ 3389 ];
  };
}

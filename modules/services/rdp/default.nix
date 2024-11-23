{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-rdp;
in
{
  options = {
    services.az-rdp.enable = lib.mkEnableOption "enable rdp";
  };

  config = lib.mkIf cfg.enable {
    services.xrdp = {
      enable = true;
      defaultWindowManager = "${pkgs.qtile-unwrapped}/bin/qtile start";
    };
    networking.firewall.allowedTCPPorts = [ 3389 ];
  };
}

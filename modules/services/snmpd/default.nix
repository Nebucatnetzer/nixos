{
  config,
  lib,
  ...
}:
let
  cfg = config.services.az-snmpd;
in
{
  options = {
    services.az-snmpd.enable = lib.mkEnableOption "Enable SNMPD.";
  };

  config = lib.mkIf cfg.enable {
    services.snmpd = {
      enable = true;
      openFirewall = true;
      configText = ''
        sysLocation    Zweili.org Datacenter
        sysContact     Nebucatnetzer
        sysServices    72

        rocommunity public 10.7.89.0/24
      '';
    };
  };
}

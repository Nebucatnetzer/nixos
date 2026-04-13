{ pkgs, ... }:
{
  services.snmpd = {
    enable = true;
    openFirewall = true;
    configText = ''
      sysLocation    Zweili.org Datacenter
      sysContact     Nebucatnetzer
      sysServices    72

      rocommunity public 10.7.89.0/24
      rocommunity public 10.70.89.0/24
      rocommunity public 127.0.0.0/24
    '';
  };
}

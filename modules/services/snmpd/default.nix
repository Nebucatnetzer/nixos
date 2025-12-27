{ pkgs, ... }:
let
  # Fixes this issue: https://github.com/net-snmp/net-snmp/issues/829
  # Only required on net-snmp <5.9.5
  patched-snmp = pkgs.net-snmp.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      (pkgs.fetchpatch {
        url = "https://github.com/net-snmp/net-snmp/commit/49d60ba57f4b462df7dc5fd5b38b4425dab0982c.diff";
        hash = "sha256-ZSF16RacrHddH50inHdmDYnu+fDS5eZd4PgK62s5C4g=";
      })
    ];
  });
in
{
  services.snmpd = {
    enable = true;
    package = patched-snmp;
    openFirewall = true;
    configText = ''
      sysLocation    Zweili.org Datacenter
      sysContact     Nebucatnetzer
      sysServices    72

      rocommunity public 10.7.89.0/24
    '';
  };
}

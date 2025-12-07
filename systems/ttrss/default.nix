{ hostname }:
{ config, inputs, ... }:
let
  rssBridgeDomain = "rss-bridge.zweili.org";
  domains = [
    { fqdn = "${config.services.freshrss.virtualHost}"; }
    { fqdn = rssBridgeDomain; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate" {
    inherit domains;
  };
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.115";
  };
  rssBridgeModule = import "${inputs.self}/modules/services/rss-bridge" {
    domain = rssBridgeDomain;
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/freshrss"
    librenmsCertificateModule
    raspiEthernet
    rssBridgeModule
  ];
  services.az-restic-client-server-mysql = {
    enable = true;
    path = config.services.freshrss.dataDir;
    tag = "freshrss";
    time = "23:00";
  };
}

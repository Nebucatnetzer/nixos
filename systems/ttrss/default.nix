{ hostname }:
{ config, inputs, ... }:
let
  domains = [
    { fqdn = "${config.services.freshrss.virtualHost}"; }
    { fqdn = rssBridgeDomain; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  resticClientServerMysql = import "${inputs.self}/modules/services/restic-client-server-mysql";
  rssBridgeDomain = "rss-bridge.zweili.org";
  rssBridgeModule = import "${inputs.self}/modules/services/rss-bridge";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/freshrss"
    raspi4Configs.diskLayouts.singleSdCard
    (librenmsCertificateModule {
      inherit domains;
    })
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.115";
    })
    (resticClientServerMysql {
      path = config.services.freshrss.dataDir;
      tag = "freshrss";
      time = "23:00";
    })
    (rssBridgeModule {
      domain = rssBridgeDomain;
    })
  ];
}

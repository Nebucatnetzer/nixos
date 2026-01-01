{ hostname }:
{ inputs, ... }:
let
  domain = "git.2li.ch";
  domains = [
    { fqdn = "${domain}"; }
  ];
  gitea = import "${inputs.self}/modules/services/gitea" { inherit domain; };
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate" {
    inherit domains;
  };
  nginxProxy = import "${inputs.self}/modules/services/nginx-proxy";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.109";
  };
  resticClientServerMysql = import "${inputs.self}/modules/services/restic-client-server-mysql";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    raspi4Configs.diskLayouts.singleSdCard
    gitea
    librenmsCertificateModule
    (nginxProxy { inherit domain; })
    raspiEthernet
    (resticClientServerMysql {
      path = "/mnt/server-data";
      time = "00:30";
    })
  ];
}

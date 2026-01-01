{ hostname }:
{ inputs, ... }:
let
  domain = "git.2li.ch";
  domains = [
    { fqdn = "${domain}"; }
  ];
  gitea = import "${inputs.self}/modules/services/gitea";
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate";
  nginxProxy = import "${inputs.self}/modules/services/nginx-proxy";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  resticClientServerMysql = import "${inputs.self}/modules/services/restic-client-server-mysql";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    (gitea { inherit domain; })
    (librenmsCertificateModule { inherit domains; })
    (nginxProxy { inherit domain; })
    raspi4Configs.diskLayouts.singleSdCard
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.109";
    })
    (resticClientServerMysql {
      path = "/mnt/server-data";
      time = "00:30";
    })
  ];
}

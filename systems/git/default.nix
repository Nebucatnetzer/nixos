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
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.109";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/az-restic-client-server-mysql"
    gitea
    librenmsCertificateModule
    (nginxProxy { inherit domain; })
    raspiEthernet
  ];
  services = {
    az-restic-client-server-mysql = {
      enable = true;
      path = "/mnt/server-data";
      time = "00:30";
    };
  };
}

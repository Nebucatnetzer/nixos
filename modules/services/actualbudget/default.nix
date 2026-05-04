{
  dataDirectory,
  domain,
  name,
  port,
}:
{
  inputs,
  ...
}:
let
  domains = [
    { fqdn = "${domain}"; }
  ];
  librenmsCertificateModule = import "${inputs.self}/modules/services/librenms-certificate" {
    inherit domains;
  };
in
{
  imports = [
    "${inputs.self}/modules/services/docker"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/telegram-notifications"
    librenmsCertificateModule
  ];
  # Webserver setup
  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${builtins.toString port}";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
  virtualisation.oci-containers = {
    backend = "docker";
    containers."${name}" = {
      # https://github.com/actualbudget/actual/releases
      image = "ghcr.io/actualbudget/actual:26.5.0@sha256:b733ae30c70a66dc4d03577526e53575a0c04eab4f3ab6ace30934776251058c";
      autoStart = true;
      ports = [ "${builtins.toString port}:5006" ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "${dataDirectory}:/data"
      ];
      extraOptions = [ "--log-opt=tag='${domain}'" ];
    };
  };
}

{ domain, inputs, ... }:
{
  imports = [
    "${inputs.self}/modules/nginx-acme-base"
  ];
  services.nginx = {
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
}

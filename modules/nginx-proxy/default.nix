{ domain, port ? "8080" }: { inputs, ... }: {
  services = {
    az-acme-base.enable = true;
    nginx = {
      appendHttpConfig = ''
        # Disable embedding as a frame
        add_header X-Frame-Options DENY;
      '';
      recommendedProxySettings = true;
      virtualHosts."${domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${port}";
          proxyWebsockets = true; # needed if you need to use WebSocket
        };
      };
    };
  };
}

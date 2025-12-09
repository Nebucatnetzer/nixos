{
  config,
  inputs,
  pkgs,
  ...
}:
let
  dataDirectory = "/var/lib/eactual";
  domain = "eactual.zweili.org";
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
  networking.firewall.allowedTCPPorts = [ 443 ];
  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:5007";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
  virtualisation.oci-containers = {
    backend = "docker";
    containers."eactualbudget" = {
      # https://github.com/actualbudget/actual/releases
      image = "ghcr.io/actualbudget/actual-server:25.12.0@sha256:7f38b450cc3b7523dd5377792895e58b9cf333769e5a27f89e643b6f5fc60565";
      autoStart = true;
      ports = [ "5007:5006" ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "${dataDirectory}:/data"
      ];
      extraOptions = [ "--log-opt=tag='eactualbudget'" ];
    };
  };

  # Backups
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";
  systemd.timers."restic-backups-eactual" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups-eactual.service" ];
    timerConfig = {
      OnCalendar = "22:45";
    };
  };

  systemd.services."restic-backups-eactual" = {
    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
    environment = {
      RESTIC_PASSWORD_FILE = config.age.secrets.resticKey.path;
      RESTIC_REPOSITORY = "rest:http://10.7.89.30:8000";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
        --tag "actualbudget" ${dataDirectory}

      ${pkgs.restic}/bin/restic forget \
        --tag "actualbudget" \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2
    '';
  };
}

{
  config,
  inputs,
  pkgs,
  ...
}:
let
  dataDirectory = "/var/lib/actualbudget";
  domain = "actual.zweili.org";
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
        proxyPass = "http://127.0.0.1:5006";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
  virtualisation.oci-containers = {
    backend = "docker";
    containers."actualbudget" = {
      # https://github.com/actualbudget/actual/releases
      image = "ghcr.io/actualbudget/actual-server:25.11.0@sha256:8f72d73e68958566850325c1f9fe780a4477340979969bd6979649dc519723ab";
      autoStart = true;
      ports = [ "5006:5006" ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "${dataDirectory}:/data"
      ];
      extraOptions = [ "--log-opt=tag='actualbudget'" ];
    };
  };

  # Backups
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";
  systemd.timers."restic-backups-actual" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups-actual.service" ];
    timerConfig = {
      OnCalendar = "22:40";
    };
  };

  systemd.services."restic-backups-actual" = {
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

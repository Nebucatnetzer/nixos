{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-actualbudget;
  dataDirectory = "/var/lib/actualbudget";
  domain = "actual.zweili.org";
in
{
  options = {
    services.az-actualbudget.enable = lib.mkEnableOption "Enable Actualbudget";
  };
  config = lib.mkIf cfg.enable {

    # Monitor certificates
    services.az-librenms-certificate = {
      enable = true;
      domains = [
        { fqdn = "${domain}"; }
      ];
    };

    # Webserver setup
    services.nginx = {
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
        # https://hub.docker.com/r/mailserver/docker-mailserver/tags
        image = "ghcr.io/actualbudget/actual-server:25.9.0@sha256:a96e38821a56843a5473204cbd3773ffee816c49c23e0a9187fb80498bd3e154";
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
    services.az-telegram-notifications.enable = true;
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
  };
}

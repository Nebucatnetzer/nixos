{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.services.az-restic-client-server-mysql;
in
{
  options = {
    services.az-restic-client-server-mysql = {
      enable = lib.mkEnableOption "Enable restic backups for MariaDB.";
      path = lib.mkOption {
        type = lib.types.path;
        description = "The directory to backup.";
      };
      tag = lib.mkOption {
        type = lib.types.str;
        description = "The tag to attach to the backups.";
        default = "home-dir";
      };
      time = lib.mkOption {
        type = lib.types.str;
        description = "The time at which the backup runs.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.az-telegram-notifications = {
      enable = true;
    };

    age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

    systemd.timers."restic-backups" = {
      wantedBy = [ "timers.target" ];
      partOf = [ "restic-backups.service" ];
      timerConfig = {
        OnCalendar = cfg.time;
      };
    };

    systemd.services."restic-backups" = {
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
          --tag ${cfg.tag} ${cfg.path}

        ${pkgs.mariadb}/bin/mariabackup --backup --user=root --stream=xbstream | \
        ${pkgs.restic}/bin/restic backup \
          --tag mariadb \
          --stdin \
          --stdin-filename mariadb.xb

        ${pkgs.restic}/bin/restic forget \
          --tag home-dir \
          --host ${config.networking.hostName} \
          --keep-daily 7 \
          --keep-weekly 5 \
          --keep-monthly 12 \
          --keep-yearly 2

        ${pkgs.restic}/bin/restic forget \
          --tag mariadb \
          --host ${config.networking.hostName} \
          --keep-daily 7 \
          --keep-weekly 5 \
          --keep-monthly 12 \
          --keep-yearly 2
      '';
    };
  };
}

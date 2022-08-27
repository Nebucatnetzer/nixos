{ config, inputs, custom, pkgs, time, ... }:
{
  imports = [
    "${inputs.self}/modules/telegram-notifications"
  ];
  systemd.timers."restic-backups-${custom.username}" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups-${custom.username}.service" ];
    timerConfig = {
      OnCalendar = time;
    };
  };

  systemd.services."restic-backups-${custom.username}" = {
    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
    environment = {
      RESTIC_PASSWORD_FILE = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
      RESTIC_REPOSITORY = "rest:http://10.7.89.30:8000";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${inputs.self}/modules/restic/excludes.txt \
        --tag home-dir /home/${custom.username}

      ${pkgs.mariadb}/bin/mysqldump --single-transaction --all-databases | \
      ${pkgs.restic}/bin/restic backup \
        --tag mariadb \
        --stdin \
        --stdin-filename all_databases.sql

      ${pkgs.restic}/bin/restic forget \
        --tag home-dir \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75

      ${pkgs.restic}/bin/restic forget \
        --tag mariadb \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75
    '';
  };
}

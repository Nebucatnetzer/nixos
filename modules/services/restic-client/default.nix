{
  mariadb ? false,
  paths ? [ ],
  postgresql ? false,
  resticSchedule ? "hourly",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  mariadbBackup = ''
    echo "Start MariaDB backup."
    ${config.services.mysql.package}/bin/mariadb-backup --backup --user=root --stream=xbstream | \
    ${pkgs.restic}/bin/restic backup \
      --tag mariadb \
      --stdin \
      --stdin-filename mariadb.xb

    echo "Forget MariaDB backup points."
    ${pkgs.restic}/bin/restic forget \
      --tag mariadb \
      --host ${config.networking.hostName} \
      --keep-daily 7 \
      --keep-weekly 5 \
      --keep-monthly 12 \
      --keep-yearly 2
  '';
  postgresBackup = ''
    echo "Start Postgresql backup."
    ${pkgs.sudo}/bin/sudo -u postgres ${config.services.postgresql.package}/bin/pg_dumpall | \
    ${pkgs.restic}/bin/restic backup \
      --tag postgres \
      --stdin \
      --stdin-filename all_databases.sql

    echo "Forget Postgresql backup points."
    ${pkgs.restic}/bin/restic forget \
      --tag postgres \
      --host ${config.networking.hostName} \
      --keep-daily 7 \
      --keep-weekly 5 \
      --keep-monthly 12 \
      --keep-yearly 2
  '';
  pathsString = lib.concatStringsSep " " paths;
in
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
  ];
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

  systemd.timers."restic-backups" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups.service" ];
    timerConfig = {
      OnCalendar = resticSchedule;
    };
  };

  systemd.services."restic-backups" = {
    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
    unitConfig = {
      ConditionACPower = true;
    };
    environment = {
      RESTIC_PASSWORD_FILE = config.age.secrets.resticKey.path;
      RESTIC_REPOSITORY = "rest:http://${config.az-hosts.gwyn.wgIp}:8123";
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      ${if mariadb then mariadbBackup else ""}

      ${if postgresql then postgresBackup else ""}

      echo "Start path backups"
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
        --one-file-system \
        --tag "paths" ${pathsString} /home/ /nix/var/nix

      echo "Forget path backup points"
      ${pkgs.restic}/bin/restic forget \
        --tag "paths" \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2
    '';
  };
}

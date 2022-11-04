{ inputs
, path
, tag ? "home-dir"
, time
}: { config, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/modules/telegram-notifications" { inherit inputs; })
  ];

  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

  systemd.timers."restic-backups" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups.service" ];
    timerConfig = {
      OnCalendar = time;
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
        --exclude-file=${inputs.self}/modules/restic/excludes.txt \
        --tag ${tag} ${path}

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

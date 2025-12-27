{
  path,
  tag ? "home-dir",
  time,
}:
{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
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
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      echo "Start data backup."
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
        --tag ${tag} ${path} /nix/var/nix

      echo "Start DB backup."
      ${pkgs.mariadb_114}/bin/mariadb-backup --backup --user=root --stream=xbstream | \
      ${pkgs.restic}/bin/restic backup \
        --tag mariadb \
        --stdin \
        --stdin-filename mariadb.xb

      echo "Forget data backup points."
      ${pkgs.restic}/bin/restic forget \
        --tag ${tag} \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2

      echo "Forget DB backup points."
      ${pkgs.restic}/bin/restic forget \
        --tag mariadb \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2

      echo "Backups are finished."
    '';
  };
}

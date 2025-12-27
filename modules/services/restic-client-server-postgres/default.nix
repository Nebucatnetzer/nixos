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
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
        --tag ${tag} ${path} /nix/var/nix

      ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql_15}/bin/pg_dumpall | \
      ${pkgs.restic}/bin/restic backup \
        --tag postgres \
        --stdin \
        --stdin-filename all_databases.sql

      ${pkgs.restic}/bin/restic forget \
        --tag ${tag} \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2

      ${pkgs.restic}/bin/restic forget \
        --tag postgres \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2
    '';
  };
}

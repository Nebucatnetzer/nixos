{ custom
, hostname
, inputs
, path ? "/home/${custom.username}"
, tag ? "home-dir"
, time
}: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/modules/telegram-notifications" { inherit inputs; })
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
        --tag ${tag} ${path}

      ${pkgs.restic}/bin/restic forget \
        --tag home-dir \
        --host ${hostname} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75
    '';
  };
}

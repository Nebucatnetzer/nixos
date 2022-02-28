{ config, inputs, custom, time, ... }:
{
  systemd.timers."restic-backups-${custom.username}" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups-${custom.username}.service" ];
    timerConfig = {
      OnCalendar = time;
    };
  };

  systemd.services."restic-backups-${custom.username}" = {
    serviceConfig = {
      User = root;
      Type = "oneshot";
    };
    environment = {
      RESTIC_PASSWORD_FILE = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
      RESTIC_REPOSITORY = "rest:http://10.7.89.30:8000";
    };
    script = ''
      ${pkgs.restic}/bin/restic \
      --exclude-file=${inputs.self}/modules/restic/excludes.txt \
      backup /home/${custom.username} \

      ${pkgs.restic}/bin/restic \
      forget \
        --host ${config.networking.hostName} \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75 \
    '';
  };
}

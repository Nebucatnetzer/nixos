{ config, inputs, custom, pkgs, ... }:
let
  password_file = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
  repository = "rest:http://10.7.89.30:8000";
in
{
  environment.systemPackages = with pkgs;
    [
      restic
    ];

  systemd.timers."restic-backups-${custom.username}" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-backups-${custom.username}.service" ];
    timerConfig = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "15min";
    };
  };

  systemd.services."restic-backups-${custom.username}" = {
    unitConfig.ConditionACPower = true;
    serviceConfig = {
      User = custom.username;
      Type = "oneshot";
    };
    environment = {
      RESTIC_PASSWORD_FILE = password_file;
      RESTIC_REPOSITORY = repository;
    };
    script = ''
      ${pkgs.restic}/bin/restic \
      --exclude-file=${inputs.self}/modules/restic/excludes.txt \
      backup /home/${custom.username} \

      ${pkgs.restic}/bin/restic \
      forget \
        --host ${config.networking.hostName} \
        --keep-hourly 25 \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75 \
    '';
  };
  environment.shellAliases = {
    restic-list = ''
      restic \
        --repo ${repository} \
        --password-file ${password_file} \
        snapshots --host ${config.networking.hostName}
    '';
    restic-unlock = ''
      restic \
        --repo ${repository} \
        --password-file ${password_file} \
        unlock
    '';
    restic-mount = ''
      mkdir -p /tmp/restic && \
      restic \
        --repo ${repository} \
        --password-file ${password_file} \
        --host ${config.networking.hostName} \
        mount /tmp/restic
    '';
    restic-mount-all = ''
      mkdir -p /tmp/restic && \
      restic \
        --repo ${repository} \
        --password-file ${password_file} \
        mount /tmp/restic
    '';
    restic-forget = ''
      restic \
        --repo ${repository} \
        --password-file ${password_file} \
        forget $1'';
  };
}

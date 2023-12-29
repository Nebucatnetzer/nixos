{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.services.az-restic-client-desktop;
  password_file = config.age.secrets.resticKey.path;
  backup_server = "10.7.89.30";
  repository = "rest:http://${backup_server}:8000";
in
{
  options = {
    services.az-restic-client-desktop.enable = lib.mkEnableOption "Enable restic backups";
  };
  config = lib.mkIf cfg.enable {
    services.az-telegram-notifications.enable = true;
    age.secrets.infomaniakEnv = {
      file = "${inputs.self}/scrts/infomaniak_env.age";
      mode = "600";
      owner = config.az-username;
      group = "users";
    };
    age.secrets.resticKey = {
      file = "${inputs.self}/scrts/restic.key.age";
      mode = "600";
      owner = config.az-username;
      group = "users";
    };

    systemd.timers."restic-backups-${config.az-username}" = {
      wantedBy = [ "timers.target" ];
      partOf = [ "restic-backups-${config.az-username}.service" ];
      timerConfig = {
        OnCalendar = "hourly";
        RandomizedDelaySec = "15min";
      };
    };

    systemd.services."restic-backups-${config.az-username}" = {
      unitConfig.ConditionACPower = true;
      serviceConfig = {
        User = config.az-username;
        Type = "oneshot";
      };
      environment = {
        RESTIC_PASSWORD_FILE = password_file;
        RESTIC_REPOSITORY = repository;
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      script = ''
        # first make sure we can reach the backup server
        if ${pkgs.iputils}/bin/ping -c 1 ${backup_server} >/dev/null; then
            ${pkgs.restic}/bin/restic \
            --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
            --tag home-dir \
            backup /home/${config.az-username}

            ${pkgs.restic}/bin/restic \
            forget \
            --host ${config.networking.hostName} \
            --keep-hourly 25 \
            --keep-daily 7 \
            --keep-weekly 5 \
            --keep-monthly 12 \
            --keep-yearly 2 \
        else
          echo "Backup server unreachable."
        fi
      '';
    };
  };
}

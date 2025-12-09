{
  config,
  inputs,
  pkgs,
  ...
}:
let
  password_file = config.age.secrets.resticKey.path;
  repository = "rest:http://10.7.89.30:8000";
in
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
  ];
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
      ${pkgs.restic}/bin/restic \
        --exclude-file=${inputs.self}/modules/misc/restic-client/excludes.txt \
        --tag home-dir \
        backup /home/${config.az-username} /nix/var/nix

      ${pkgs.restic}/bin/restic \
      forget \
        --host ${config.networking.hostName} \
        --keep-hourly 25 \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 2 \
    '';
  };
}

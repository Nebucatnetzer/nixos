{
  # Global restic options, e.g. -o rclone.program=... for a Hetzner Storage Box target.
  extraResticArgs ? [ ],
  mariadb ? false,
  # Distinguishes the timer and service names, so the module can be instantiated more
  # than once on the same host.
  name ? "backups",
  paths ? [ ],
  # null means the rest server on gwyn.
  repository ? null,
  requireMountpoints ? [ ],
  retryLock ? "10m",
  resticSchedule ? "hourly",
  # Appended to paths. An archive job sets this to [ ] and names its subvolumes instead:
  # --one-file-system stops at btrfs subvolume boundaries, so each one needs its own entry.
  systemPaths ? [
    "/home/"
    "/nix/var/nix"
  ],
  tag ? "paths",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  # Hours without a recorded backup before the age check alerts.
  maxBackupAge = 48;
  unitName = "restic-${name}";

  ageGuard = pkgs.callPackage "${inputs.self}/modules/misc/restic/age_guard.nix" {
    maxAge = maxBackupAge;
    name = "${unitName}-age";
    inherit sendToTelegram;
    # Named by host and job, because all three hosts alert into the same chat and a quiet
    # client has to be identifiable from the message alone.
    tiers = [
      {
        label = "${config.networking.hostName} ${name}";
        paths = [ stampFile ];
      }
    ];
  };

  sendToTelegram = pkgs.callPackage "${telegramNotifications}/send_to_telegram.nix" {
    envFile = config.age.secrets.telegramNotifyEnv.path;
  };

  # StateDirectory rather than a directory shared with the other jobs: systemd then
  # creates it owned by the unit's own user, so no two uids have to share one directory.
  stateDirectory = "restic-age-${name}";
  stampFile = "/var/lib/${stateDirectory}/last-success";

  telegramNotifications = "${inputs.self}/modules/services/telegram-notifications";

  usesRestServer = repository == null;
  restServerIp = config.az-hosts.gwyn.wgIp;
  restServerPort = "8123";
  resticRepository =
    if usesRestServer then "rest:http://${restServerIp}:${restServerPort}" else repository;

  restic = lib.concatStringsSep " " (
    [
      "${pkgs.restic}/bin/restic"
      "--retry-lock ${retryLock}"
    ]
    ++ extraResticArgs
  );

  mariadbBackup = lib.optionalString mariadb ''
    echo "Start MariaDB backup."
    ${config.services.mysql.package}/bin/mariadb-backup --backup --user=root --stream=xbstream | \
    ${restic} backup \
      --tag mariadb \
      --stdin \
      --stdin-filename mariadb.xb
  '';

  # Only for the rest server: a laptop away from home skips the run instead of failing.
  probeScript = lib.optionalString usesRestServer ''
    if ! ${pkgs.netcat}/bin/nc -zw 2 ${restServerIp} ${restServerPort} >/dev/null 2>&1; then
      echo "Target server ${restServerIp}:${restServerPort} is unreachable. Backup skipped."
      exit 0
    fi
  '';
  mountpointGuard = lib.concatMapStrings (mountPath: ''
    if ! ${pkgs.util-linux}/bin/mountpoint --quiet ${mountPath}; then
      echo "${mountPath} is not a mountpoint. Backup aborted."
      exit 1
    fi
  '') requireMountpoints;
in
{
  imports = [
    telegramNotifications
  ];
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

  systemd.timers."${unitName}" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "${unitName}.service" ];
    timerConfig = {
      OnCalendar = resticSchedule;
    };
  };

  systemd.services."${unitName}" = {
    serviceConfig = {
      # Root's HOME would put restic's cache on the root filesystem, and a remote repo
      # without a cache re-reads the whole index every run.
      CacheDirectory = "restic";
      # Holds the stamp the age check below dates the backup from.
      StateDirectory = stateDirectory;
      User = "root";
      Type = "oneshot";
    };
    unitConfig = {
      ConditionACPower = true;
    };
    environment = {
      RESTIC_CACHE_DIR = "/var/cache/restic";
      RESTIC_PASSWORD_FILE = config.age.secrets.resticKey.path;
      RESTIC_REPOSITORY = resticRepository;
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      ${probeScript}

      ${mountpointGuard}

      ${mariadbBackup}

      echo "Start path backups"
      ${restic} backup \
        --exclude-file=${inputs.self}/modules/misc/restic/client_excludes.txt \
        --one-file-system \
        --tag "${tag}" ${lib.concatStringsSep " " (paths ++ systemPaths)}

      # NixOS runs unit scripts under set -e, so this is only reached when the whole job
      # succeeded. Where there is a probe it exits 0 on an unreachable repository, which
      # stops short of here on purpose: skipped runs have to read as a stale backup.
      ${pkgs.coreutils}/bin/touch ${stampFile}
    '';
  };

  # Deliberately without the backup unit's ConditionACPower: a laptop that has been on
  # battery for two days is exactly the case this has to report.
  systemd.services."${unitName}-age" = {
    description = "Alert when ${unitName} has recorded nothing for ${toString maxBackupAge} hours";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = lib.getExe ageGuard;
      # Shared with the backup unit above, so the check can run and report nothing on
      # record before the first backup has ever written the stamp.
      StateDirectory = stateDirectory;
      Type = "oneshot";
      User = "root";
    };
  };

  systemd.timers."${unitName}-age" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "${unitName}-age.service" ];
    timerConfig = {
      # Daily is enough for a threshold measured in days.
      OnCalendar = "*-*-* 12:00:00";
      # So a host that was off catches up on boot rather than skipping the day.
      Persistent = true;
    };
  };
}

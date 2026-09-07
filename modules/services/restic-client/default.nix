{
  # When the age check runs. Daily is enough for a threshold measured in days.
  ageSchedule ? "*-*-* 12:00:00",
  # Patterns for this instantiation only. The shared excludes.txt is read by every host
  # and would be read by the archive job too, and restic matches unanchored patterns at
  # any depth, so a bare "Music" line there would silently drop the FLAC masters from the
  # archive backup. Anchor everything put here.
  excludes ? [ ],
  # Global restic options, e.g. -o rclone.program=... for a Hetzner Storage Box target.
  extraResticArgs ? [ ],
  mariadb ? false,
  # Hours without a recorded backup before the age check alerts.
  maxBackupAge ? 48,
  # Distinguishes the timer and service names, so the module can be instantiated more
  # than once on the same host.
  name ? "backups",
  paths ? [ ],
  postgresql ? false,
  # Reachability probe. Defaults to the rest server when the default repository is used;
  # a caller naming its own repository names its own probe, or gets none.
  probeHost ? null,
  probePort ? null,
  # null means the rest server on gwyn.
  repository ? null,
  requireMountpoints ? [ ],
  retryLock ? "10m",
  resticSchedule ? "hourly",
  # An empty list means no forget at all, which is what an archive wants: forget removes
  # snapshots, so a bounded policy eventually ages out every snapshot holding a file that
  # was deleted by accident.
  retention ? [
    "--keep-daily 7"
    "--keep-weekly 5"
    "--keep-monthly 12"
    "--keep-yearly 2"
  ],
  # Appended to paths. An archive job sets this to [ ] and names its subvolumes instead:
  # --one-file-system stops at btrfs subvolume boundaries, so each one needs its own entry.
  systemPaths ? [
    "/home/"
    "/nix/var/nix"
  ],
  tag ? "paths",
  useSharedExcludes ? true,
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  unitName = "restic-${name}";

  ageGuard = pkgs.callPackage "${inputs.self}/modules/misc/restic-age/age_guard.nix" {
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

  usesDefaultRepository = repository == null;
  resticRepository =
    if usesDefaultRepository then "rest:http://${config.az-hosts.gwyn.wgIp}:8123" else repository;

  probeHostname =
    if probeHost != null then
      probeHost
    else if usesDefaultRepository then
      config.az-hosts.gwyn.wgIp
    else
      null;
  probePortNumber =
    if probePort != null then
      toString probePort
    else if usesDefaultRepository then
      "8123"
    else
      null;

  sharedExcludeFile = "${inputs.self}/modules/misc/restic-client/excludes.txt";
  ownExcludeFile = pkgs.writeText "${unitName}-excludes.txt" (
    lib.concatStringsSep "\n" excludes + "\n"
  );
  excludeArgs =
    lib.optional useSharedExcludes "--exclude-file=${sharedExcludeFile}"
    ++ lib.optional (excludes != [ ]) "--exclude-file=${ownExcludeFile}";

  restic = lib.concatStringsSep " " (
    [ "${pkgs.restic}/bin/restic" ] ++ [ "--retry-lock ${retryLock}" ] ++ extraResticArgs
  );
  continued = " \\\n  ";

  forget =
    forgetTag:
    lib.optionalString (retention != [ ]) ''
      echo "Forget ${forgetTag} backup points."
      ${restic} forget \
        --tag ${forgetTag} \
        --host ${config.networking.hostName} \
        ${lib.concatStringsSep continued retention}
    '';

  mariadbBackup = ''
    echo "Start MariaDB backup."
    ${config.services.mysql.package}/bin/mariadb-backup --backup --user=root --stream=xbstream | \
    ${restic} backup \
      --tag mariadb \
      --stdin \
      --stdin-filename mariadb.xb

    ${forget "mariadb"}
  '';
  postgresBackup = ''
    echo "Start Postgresql backup."
    ${pkgs.sudo}/bin/sudo -u postgres ${config.services.postgresql.package}/bin/pg_dumpall | \
    ${restic} backup \
      --tag postgres \
      --stdin \
      --stdin-filename all_databases.sql

    ${forget "postgres"}
  '';

  probeScript = lib.optionalString (probeHostname != null) ''
    BACKUP_SERVER=${probeHostname}
    PORT=${probePortNumber}
    TIMEOUT=2
    if ! ${pkgs.netcat}/bin/nc -vzw "$TIMEOUT" "$BACKUP_SERVER" "$PORT" >/dev/null 2>&1; then
      echo "Target server $BACKUP_SERVER:$PORT is unreachable. Backup skipped."
      exit 0
    fi
  '';
  mountpointGuard = lib.concatMapStrings (mountPath: ''
    if ! ${pkgs.util-linux}/bin/mountpoint --quiet ${mountPath}; then
      echo "${mountPath} is not a mountpoint. Backup aborted."
      exit 1
    fi
  '') requireMountpoints;

  pathsString = lib.concatStringsSep " " (paths ++ systemPaths);
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

      ${if mariadb then mariadbBackup else ""}

      ${if postgresql then postgresBackup else ""}

      echo "Start path backups"
      ${restic} backup \
        ${lib.concatStringsSep continued excludeArgs} \
        --one-file-system \
        --tag "${tag}" ${pathsString}

      ${forget tag}

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
      OnCalendar = ageSchedule;
      # So a host that was off catches up on boot rather than skipping the day.
      Persistent = true;
    };
  };
}

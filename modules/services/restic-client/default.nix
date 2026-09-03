{
  # Patterns for this instantiation only. The shared excludes.txt is read by every host
  # and would be read by the archive job too, and restic matches unanchored patterns at
  # any depth, so a bare "Music" line there would silently drop the FLAC masters from the
  # archive backup. Anchor everything put here.
  excludes ? [ ],
  # Global restic options, e.g. -o rclone.program=... for a Hetzner Storage Box target.
  extraResticArgs ? [ ],
  mariadb ? false,
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

  restic = lib.concatStringsSep " " ([ "${pkgs.restic}/bin/restic" ] ++ extraResticArgs);
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

  pathsString = lib.concatStringsSep " " (paths ++ systemPaths);
in
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
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
      User = "root";
      Type = "oneshot";
    };
    unitConfig = {
      ConditionACPower = true;
    };
    environment = {
      RESTIC_PASSWORD_FILE = config.age.secrets.resticKey.path;
      RESTIC_REPOSITORY = resticRepository;
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      ${probeScript}

      ${if mariadb then mariadbBackup else ""}

      ${if postgresql then postgresBackup else ""}

      echo "Start path backups"
      ${restic} backup \
        ${lib.concatStringsSep continued excludeArgs} \
        --one-file-system \
        --tag "${tag}" ${pathsString}

      ${forget tag}
    '';
  };
}

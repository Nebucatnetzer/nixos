{
  repository ? "/var/lib/restic-server",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  # Not the /var/cache/restic the root client jobs use: two uids sharing one cache
  # directory is the same ownership mix this module exists to avoid.
  cacheDirectory = "/var/cache/restic-server";
  # --retry-lock on everything: the chains overlap by design, a copy holds a shared lock
  # on both repos while a prune needs an exclusive one, so waiting beats failing.
  localRestic = lib.concatStringsSep " " [
    "${pkgs.restic}/bin/restic"
    "--repo ${repository}"
    "--password-file ${passwordFile}"
    "--retry-lock 30m"
  ];
  # Every unit here runs as restic, the uid that owns the repository, so that nothing
  # else ever writes into it: a file owned by another uid is one the rest server cannot
  # open, and clients then get a 500 for that file. The Storage Box key exists a second
  # time for this user, because ssh refuses a key that is group readable.
  offsiteEnvironment = {
    RESTIC_CACHE_DIR = cacheDirectory;
    RESTIC_PASSWORD_FILE = passwordFile;
    RESTIC_REPOSITORY = config.az-storage-box.repository;
  };
  # host,paths,tags is the finest grouping restic offers, so one host going quiet can
  # never age out another host's snapshots.
  offsiteForget = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (forgetTag: policy: ''
      echo "Forget ${forgetTag} snapshots offsite."
      ${offsiteResticWritable} forget \
        --tag ${forgetTag} \
        --group-by host,paths,tags \
        ${lib.concatStringsSep " \\\n  " policy}
    '') offsiteRetention
  );
  # Longer than the client-side policy on purpose, so forget offsite only ever removes
  # what the local repo already dropped. yearly is unlimited, a literal restic accepts,
  # because this is the copy that has to answer a deletion noticed years later.
  offsiteKeepPolicy = [
    "--keep-daily 7"
    "--keep-weekly 5"
    "--keep-monthly 24"
    "--keep-yearly unlimited"
  ];
  offsiteRestic = lib.concatStringsSep " " (
    [
      "${pkgs.restic}/bin/restic"
      "--retry-lock 30m"
    ]
    ++ config.az-storage-box.extraResticArgs
  );
  # forget and prune get a 403 through the append-only transport, so the maintenance unit
  # is the one place that gets the writable one.
  offsiteResticWritable = lib.concatStringsSep " " (
    [
      "${pkgs.restic}/bin/restic"
      "--retry-lock 30m"
    ]
    ++ config.az-storage-box.pruneResticArgs
  );
  # Per tag, because a tag absent here is never forgotten..
  offsiteRetention = {
    archive = offsiteKeepPolicy;
    paths = offsiteKeepPolicy;
    mariadb = offsiteKeepPolicy;
  };
  passwordFile = config.age.secrets.resticKey.path;
  # Reports for itself rather than through onFailure, so the message names the disk
  # instead of being a systemctl status dump. Still exits non-zero, so the unit also
  # shows up in systemctl --failed.
  repositoryGuard = pkgs.writeShellApplication {
    name = "restic-repository-guard";
    runtimeInputs = [ pkgs.util-linux ];
    text = ''
      if mountpoint -q ${repository}; then
        exit 0
      fi

      ${sendToTelegram}/bin/send-to-telegram "⚠ Restic repository disk is missing ⚠
      ${repository} is not a mountpoint, so nothing is being backed up."
      exit 1
    '';
  };
  # The repository lives on its own disk, mounted with nofail. nixpkgs' rest-server module
  # sets createHome on the restic user, so without this a missing disk would leave every
  # unit writing into a fresh empty repository on the root filesystem, with no error.
  requireRepoMount = {
    unitConfig.RequiresMountsFor = repository;
  };
  # The restic user's home is the repository directory itself, so an explicit cache
  # directory is what keeps restic's cache from landing inside the repo.
  resticIdentity = {
    CacheDirectory = "restic-server";
    Group = "restic";
    Type = "oneshot";
    User = "restic";
  };
  # Interactive use has to be restic as well. A restic run as root or as the main user
  # against the repository path leaves files the rest server cannot open, which is what
  # broke every client on 2026-09-07.
  resticLocal = pkgs.writeShellApplication {
    name = "restic-local";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.sudo
    ];
    text = ''
      exec sudo --user=restic \
        env RESTIC_CACHE_DIR=${cacheDirectory} \
        ${pkgs.restic}/bin/restic \
          --repo ${repository} \
          --password-file ${passwordFile} \
          --retry-lock 30m \
          "$@"
    '';
  };
  sendToTelegram = pkgs.callPackage "${telegramNotifications}/send_to_telegram.nix" {
    envFile = config.age.secrets.telegramNotifyEnv.path;
  };
  telegramNotifications = "${inputs.self}/modules/services/telegram-notifications";
in
{
  imports = [
    telegramNotifications
  ];
  environment.systemPackages = [
    pkgs.restic
    resticLocal
  ];

  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
    listenAddress = "${config.az-hosts.gwyn.wgIp}:8123";
  };
  networking.firewall.allowedTCPPorts = [ 8123 ];

  # /var/cache is root owned, so the restic user cannot create its cache directory
  # itself. The units get it from CacheDirectory, restic-local needs it to exist.
  systemd.tmpfiles.rules = [ "d ${cacheDirectory} 0755 restic restic -" ];

  # The units below carry RequiresMountsFor, which aborts their start job as a dependency
  # failure when the disk is absent. That leaves them inactive rather than failed, so they
  # never run and a missing disk would pass unnoticed. This unit is deliberately free of
  # that dependency: it runs, notifies, and fails. It wants the network so that the alert
  # at boot is actually deliverable.
  systemd.services.restic-repository-mounted = {
    description = "Check that the restic repository disk is mounted";
    after = [
      "local-fs.target"
      "network-online.target"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = lib.getExe repositoryGuard;
    };
  };

  systemd.timers.restic-repository-mounted = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-repository-mounted.service" ];
    timerConfig.OnCalendar = "hourly";
  };

  systemd.services.restic-rest-server = requireRepoMount;

  # Local repo: the clients push into the rest server all day, then prune, then check.
  systemd.services.restic-prune = requireRepoMount // {
    serviceConfig = resticIdentity;
    environment.RESTIC_CACHE_DIR = cacheDirectory;
    onFailure = [ "unit-status-telegram@%N.service" ];
    onSuccess = [ "restic-check.service" ];
    script = "${localRestic} prune";
  };

  systemd.timers.restic-prune = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-prune.service" ];
    timerConfig.OnCalendar = [ "*-*-* 07:00:00" ];
  };

  # No timer of its own. A check verifies what prune just rewrote, and a gap on the clock
  # cannot express that ordering. Started by prune, or by hand.
  systemd.services.restic-check = requireRepoMount // {
    serviceConfig = resticIdentity;
    environment.RESTIC_CACHE_DIR = cacheDirectory;
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = "${localRestic} check";
  };

  # Offsite repo: its own timer rather than an onSuccess off restic-prune. A local repo
  # problem must not silently stop the copy that survives the house burning down.
  systemd.services.restic-offsite-copy = requireRepoMount // {
    serviceConfig = resticIdentity;
    environment = offsiteEnvironment // {
      RESTIC_FROM_PASSWORD_FILE = passwordFile;
      RESTIC_FROM_REPOSITORY = repository;
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    onSuccess = [ "restic-offsite-check.service" ];
    script = "${offsiteRestic} copy";
  };

  systemd.timers.restic-offsite-copy = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-offsite-copy.service" ];
    timerConfig = {
      OnCalendar = [ "*-*-* 09:00:00" ];
      Persistent = true;
    };
  };

  # Quarterly forget and prune in one shot
  systemd.services.restic-offsite-prune = {
    requires = [ "restic-offsite-copy.service" ];
    after = [ "restic-offsite-copy.service" ];
    serviceConfig = resticIdentity;
    environment = offsiteEnvironment;
    onFailure = [ "unit-status-telegram@%N.service" ];
    onSuccess = [ "restic-offsite-check.service" ];
    script = ''
      ${offsiteForget}

      echo "Prune the offsite repository."
      ${offsiteResticWritable} prune
    '';
  };

  systemd.timers.restic-offsite-prune = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-offsite-prune.service" ];
    timerConfig = {
      OnCalendar = [ "*-01,04,07,10-01 09:00:00" ];
      Persistent = true;
    };
  };

  # after, not requires: the copy queues this on success, and on a prune day the prune is
  # starting at that same moment. Without the ordering the check would hold a shared lock
  # while prune wants an exclusive one, and prune would exit 11.
  systemd.services.restic-offsite-check = {
    after = [
      "restic-offsite-copy.service"
      "restic-offsite-prune.service"
    ];
    serviceConfig = resticIdentity;
    environment = offsiteEnvironment;
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = "${offsiteRestic} check";
  };
}

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
  offsiteRepo = "swift:default:/";
  passwordFile = config.age.secrets.resticKey.path;
  offsite-repo-check = pkgs.callPackage ./offsite_repo_check.nix {
    inherit (swiftStorage) envFile;
    resticPassword = passwordFile;
    resticRepo = offsiteRepo;
    inherit (swiftStorage) swiftAuthUrl;
  };
  offsite-repo-sync = pkgs.callPackage ./offsite_repo_sync.nix {
    inherit (swiftStorage) envFile;
    localResticRepo = repository;
    inherit (swiftStorage) swiftAuthUrl;
    swiftRegion = "RegionOne";
  };
  swiftStorage = import "${inputs.self}/modules/misc/swift-storage" config;

  # The repository lives on its own disk, mounted with nofail. nixpkgs' rest-server module
  # sets createHome on the restic user, so without this a missing disk would leave every
  # unit writing into a fresh empty repository on the root filesystem, with no error.
  requireRepoMount = {
    unitConfig.RequiresMountsFor = repository;
  };

  telegramNotifications = "${inputs.self}/modules/services/telegram-notifications";
  sendToTelegram = pkgs.callPackage "${telegramNotifications}/send_to_telegram.nix" {
    envFile = config.age.secrets.telegramNotifyEnv.path;
  };

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
in
{
  imports = [
    telegramNotifications
  ];
  environment.systemPackages = [
    pkgs.restic
  ];

  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
    listenAddress = "${config.az-hosts.gwyn.wgIp}:8123";
  };
  networking.firewall.allowedTCPPorts = [ 8123 ];

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

  systemd.services.restic-prune = requireRepoMount // {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    onSuccess = [ "restic-offsite-sync.service" ];
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${config.age.secrets.resticKey.path} \
      prune \
    '';
  };

  systemd.services."restic-offsite-sync" = requireRepoMount // {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    onSuccess = [ "restic-offsite-check.service" ];
    script = "${offsite-repo-sync}/bin/restic-offsite-sync";
  };

  systemd.services."restic-offsite-check" = {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = "${offsite-repo-check}/bin/restic-offsite-check";
  };

  systemd.timers.restic-prune = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-prune.service" ];
    timerConfig.OnCalendar = [ "*-*-* 08:00:00" ];
  };

  systemd.services.restic-check = requireRepoMount // {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${config.age.secrets.resticKey.path} \
      check \
    '';
  };
  systemd.timers.restic-check = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-check.service" ];
    timerConfig.OnCalendar = [ "*-*-* 07:00:00" ];
  };
}

{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-restic-server;
  offsite-repo-sync = pkgs.callPackage ./offsite_repo_sync.nix {
    envFile = swiftStorage.envFile;
    localResticRepo = config.services.az-restic-server.repository;
    swiftAuthUrl = swiftStorage.swiftAuthUrl;
    swiftRegion = "RegionOne";
  };
  swiftStorage = import "${inputs.self}/modules/misc/swift-storage" config;

in
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
  ];
  options = {
    services.az-restic-server = {
      enable = lib.mkEnableOption "Enable a restic server.";
      repository = lib.mkOption {
        type = lib.types.path;
        description = "The directory where your backups get stored.";
        default = "/var/lib/restic-server";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.resticKey = {
      file = "${inputs.self}/scrts/restic.key.age";
      mode = "440";
      owner = "restic";
      group = "restic";
    };

    environment.systemPackages = [
      pkgs.restic
      offsite-repo-sync
    ];

    services.restic.server = {
      enable = true;
      dataDir = cfg.repository;
      extraFlags = [ "--no-auth" ];
    };
    networking.firewall.allowedTCPPorts = [ 8000 ];

    systemd.services.restic-prune = {
      serviceConfig = {
        Type = "oneshot";
        User = "restic";
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      script = ''
        ${pkgs.restic}/bin/restic \
        --repo ${cfg.repository} \
        --password-file ${config.age.secrets.resticKey.path} \
        prune \
      '';
    };

    systemd.timers.restic-prune = {
      wantedBy = [ "timers.target" ];
      partOf = [ "restic-prune.service" ];
      timerConfig.OnCalendar = [ "*-*-* 08:00:00" ];
    };

    systemd.services.restic-check = {
      serviceConfig = {
        Type = "oneshot";
        User = "restic";
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      script = ''
        ${pkgs.restic}/bin/restic \
        --repo ${cfg.repository} \
        --password-file ${config.age.secrets.resticKey.path} \
        check \
      '';
    };
    systemd.timers.restic-check = {
      wantedBy = [ "timers.target" ];
      partOf = [ "restic-check.service" ];
      timerConfig.OnCalendar = [ "*-*-* 07:00:00" ];
    };
  };
}

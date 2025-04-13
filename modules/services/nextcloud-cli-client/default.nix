{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-nextcloud-cli-client;
  pathToMonitor = "/home/${config.az-username}/nextcloud/";
  monitor-notes = pkgs.writeShellScriptBin "monitor-notes" ''
    ${pkgs.nextcloud-client}/bin/nextcloudcmd \
      --silent \
      --user andreas \
      --password $(cat ${config.age.secrets.nextcloudCliSecrets.path}) \
      --non-interactive \
      --path / \
      ${pathToMonitor} \
      https://nextcloud.2li.ch
  '';
in
{
  options = {
    services.az-nextcloud-cli-client.enable = lib.mkEnableOption "Enable my implementation of a Nextcloud headless client";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.nextcloud-client ];
    services.az-telegram-notifications.enable = true;

    age.secrets.nextcloudCliSecrets = {
      file = "${inputs.self}/scrts/nextcloud_cli.age";
      path = "/home/${config.az-username}/.config/nextcloud_cli/nextcloud_cli.key";
      mode = "600";
      owner = "${config.az-username}";
      group = "users";
    };

    systemd.timers."nextcloud-sync" = {
      wantedBy = [ "timers.target" ];
      partOf = [ "nextcloud-sync.service" ];
      timerConfig = {
        OnStartupSec = "5min";
        OnUnitActiveSec = "5min";
      };
    };

    systemd.services."nextcloud-sync" = {
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        User = config.az-username;
        Type = "oneshot";
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      script = "${monitor-notes}/bin/monitor-notes";
    };

    systemd.services."nextcloud-monitor" = {
      requires = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        User = config.az-username;
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
        ${pkgs.inotify-tools}/bin/inotifywait -m -r -e create,modify,delete,move --excludei '/\.' "${pathToMonitor}" |
          while read -r directory event file; do
              sleep 10
              echo "triggered because of $event on $file in $directory"
              ${monitor-notes}/bin/monitor-notes
          done
      '';
    };
  };
}

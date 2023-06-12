{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.services.az-rclone-webdav;
  pathToMonitor = "/home/${config.az-username}/10_documents/";
  syncNotes = pkgs.writeShellScriptBin "monitor-notes" ''
    ${pkgs.rclone}/bin/rclone bisync -P --remove-empty-dirs --max-delete=10 --exclude=/99_archive/** nextcloud:10_documents ${pathToMonitor}
  '';
in
{
  options = {
    services.az-rclone-webdav.enable = lib.mkEnableOption "Sync my notes with rclone over webdav.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.rclone
    ];
    services.az-telegram-notifications.enable = true;

    age.secrets.webdavSecrets = {
      file = "${inputs.self}/scrts/webdav_andreas.age";
      path = "/home/${config.az-username}/.config/rclone/rclone.conf";
      mode = "600";
      owner = "${config.az-username}";
      group = "users";
    };

    systemd.timers."rclone-webdav-sync" = {
      wantedBy = [ "timers.target" ];
      partOf = [ "rclone-webdav-sync.service" ];
      timerConfig = {
        OnStartupSec = "5min";
        OnUnitActiveSec = "5min";
      };
    };

    systemd.services."rclone-webdav-sync" = {
      after = [ "network-online.target" ];
      serviceConfig = {
        User = config.az-username;
        Type = "oneshot";
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      script = "${syncNotes}/bin/monitor-notes";
    };

    systemd.services."rclone-webdav-monitor" = {
      requires = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        User = config.az-username;
      };
      onFailure = [ "unit-status-telegram@%n.service" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
        ${pkgs.inotify-tools}/bin/inotifywait -m -r -e create,modify,delete,move "${pathToMonitor}" |
          while read -r directory event file; do
              sleep 10
              ${syncNotes}/bin/monitor-notes
          done
      '';
    };
  };
}

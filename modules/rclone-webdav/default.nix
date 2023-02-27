{ custom }: { pkgs, ... }:
let
  pathToMonitor = "/home/${custom.username}/10_documents/";
  syncNotes = pkgs.writeShellScriptBin "monitor-notes" ''
    ${pkgs.rclone}/bin/rclone bisync -P --remove-empty-dirs --max-delete=10 --exclude=/99_archive/** nextcloud:10_documents ${pathToMonitor}
  '';
in
{
  imports = [
    (import "${custom.inputs.self}/modules/telegram-notifications"
      {
        inherit custom;
      })
  ];
  age.secrets.webdavSecrets = {
    file = "${custom.inputs.self}/scrts/webdav_andreas.age";
    path = "/home/${custom.username}/.config/rclone/rclone.conf";
    mode = "600";
    owner = "${custom.username}";
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
      User = custom.username;
      Type = "oneshot";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = "${syncNotes}/bin/monitor-notes";
  };

  systemd.services."rclone-webdav-monitor" = {
    requires = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      User = custom.username;
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
}
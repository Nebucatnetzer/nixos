{ custom }: { pkgs, ... }: {
  imports = [
    (import "${custom.inputs.self}/modules/telegram-notifications"
      { inherit custom; })
  ];
  age.secrets.webdavSecrets = {
    file = "${custom.inputs.self}/scrts/webdav_andreas.age";
    path = "/home/${custom.username}/.config/rclone/rclone.conf";
    mode = "600";
    owner = "${custom.username}";
    group = "users";
  };

  systemd.timers."rclone-webdav" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "rclone-webdav.service" ];
    timerConfig = {
      OnStartupSec = "5min";
      OnUnitActiveSec = "5min";
    };
  };

  systemd.paths."rclone-webdav" = {
    enable = true;
    pathConfig = {
      PathModified = "/home/${custom.username}/10_documents";
      TriggerLimitIntervalSec = "10s";
      TriggerLimitBurst = 1;
    };
  };

  systemd.services."rclone-webdav" = {
    after = [ "network-online.target" ];
    serviceConfig = {
      User = custom.username;
      Type = "oneshot";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      ${pkgs.rclone}/bin/rclone bisync -P --stats-one-line --max-delete=10 --exclude=/99_archive/** nextcloud:10_documents /home/${custom.username}/10_documents
    '';
  };
}

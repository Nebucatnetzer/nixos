{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-clipcat;
  configFile = ./clipcatd.toml;
in
{
  options = {
    services.az-clipcat.enable = lib.mkEnableOption "Runs the clipboard service clipcat.";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.az-clipcat = {
      enable = true;
      description = "clipcat daemon";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "${pkgs.clipcat}/bin/clipcatd --no-daemon --replace --config ${configFile}";
    };
  };
}

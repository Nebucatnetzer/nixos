{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-distrobox;
in
{
  options = {
    programs.az-distrobox.enable = lib.mkEnableOption "Install distrobox and configure it to run GUI applications.";
  };
  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [
        pkgs.unstable.distrobox
        pkgs.xorg.xhost
      ];
      shellInit = ''
        [ -n "$DISPLAY" ] && xhost +si:localuser:$USER || true
      '';
    };

    home-manager.users.${config.az-username} = {
      home.file.".config/distrobox/distrobox.conf".source = ./distrobox.conf;
    };
  };
}

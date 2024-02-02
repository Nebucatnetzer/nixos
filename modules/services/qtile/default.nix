{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-qtile;
  rofi-search = pkgs.writeShellScriptBin "rofi-search" ''
    # Use rofi to get user input
    raw_user_input=$(echo "" | ${pkgs.rofi}/bin/rofi -dmenu -p "What do you want to search:")
    user_input=$(${pkgs.jq}/bin/jq --raw-output --null-input --arg x "$raw_user_input" '$x|@uri')

    # Check if user provided input
    if [ -n "$user_input" ]; then
        # Execute your command with the user input as an argument
        # Replace 'your_command' with the actual command you want to run
        $DEFAULT_BROWSER "https://duckduckgo.com/?q=$user_input"
    else
        echo "No input provided."
    fi
  '';
in
{
  options = {
    services.az-qtile.enable = lib.mkEnableOption "Enable Qtile window manager.";
  };

  config = lib.mkIf cfg.enable {
    services = {
      xserver = {
        displayManager.defaultSession = "none+qtile";
        windowManager.qtile.enable = true;
      };
    };
    home-manager.users.${config.az-username} = {
      home.file.".config/qtile/config.py".source = ./config.py;
      home.file.".config/qtile/autostart.sh".source = ./autostart.sh;
      home.packages = [
        pkgs.pulseaudio # required for volume controls in qtile
        rofi-search
      ];
      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = [ "graphical-session-pre.target" ];
        };
      };
    };
  };
}

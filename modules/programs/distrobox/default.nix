{
  config,
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  distrobox = unstable-pkgs.distrobox;
in
{
  environment = {
    systemPackages = [
      distrobox
      pkgs.xorg.xhost
    ];
    shellInit = ''
      [ -n "$DISPLAY" ] && xhost +si:localuser:$USER || true
    '';
  };

  home-manager.users.${config.az-username} = {
    home.file.".config/distrobox/distrobox.conf".source = ./distrobox.conf;
  };
}

{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.tdesktop
  ];

  home.file.".config/qtile/autostart.d/telegram.sh".source = ./telegram.sh;

}

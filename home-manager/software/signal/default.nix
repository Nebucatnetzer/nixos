{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.signal-desktop
  ];

  home.file.".config/qtile/autostart.d/signal.sh".source = ./signal.sh;
}

{ pkgs, ... }:
{
  home.packages = with pkgs; [
    autorandr
  ];

  home.file.".config/qtile/autostart.d/autorandr.sh".source = ./autorandr.sh;
}

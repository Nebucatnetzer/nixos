{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nitrogen
  ];

  home.file.".config/qtile/autostart.d/nitrogen.sh".source = ./nitrogen.sh;
}

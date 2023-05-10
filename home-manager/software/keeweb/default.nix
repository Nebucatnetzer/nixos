{ pkgs, ... }:
{
  home.packages = with pkgs; [
    keeweb
  ];

  home.file.".config/qtile/autostart.d/keeweb.sh".source = ./keeweb.sh;
}

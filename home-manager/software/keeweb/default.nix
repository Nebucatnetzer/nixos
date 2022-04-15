{ pkgs, ... }:
{
  home.packages = with pkgs; [
    keeweb
  ];

  home.file.".config/qtile/autostart.d/99_keeweb.sh".source = ./keeweb.sh;
}

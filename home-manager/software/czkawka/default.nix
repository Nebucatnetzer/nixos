{ pkgs, ... }:
{
  home.packages = with pkgs; [
    czkawka
  ];
  xdg.desktopEntries = {
    czkawka = {
      name = "Czkawka";
      exec = "${pkgs.czkawka}/bin/czkawka_gui";
      terminal = false;
      categories = [ "System" ];
    };
  };
}

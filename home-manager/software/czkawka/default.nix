{ pkgs, ... }:
{
  home.packages = with pkgs; [
    czkawka
  ];
  xdg.desktopEntries = {
    czkawka = {
      name = "Czkawka";
      exec = "czkawka_gui";
      terminal = false;
      categories = [ "System" ];
    };
  };
}

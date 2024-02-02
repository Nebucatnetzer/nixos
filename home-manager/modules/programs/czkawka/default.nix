{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-czkawka;
in
{
  options = {
    programs.az-czkawka.enable = lib.mkEnableOption "Enable czkawka.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ czkawka ];
    xdg.desktopEntries = {
      czkawka = {
        name = "Czkawka";
        exec = "${pkgs.czkawka}/bin/czkawka_gui";
        terminal = false;
        categories = [ "System" ];
      };
    };
  };
}

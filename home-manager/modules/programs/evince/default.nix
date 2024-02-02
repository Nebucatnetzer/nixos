{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-evince;
in
{
  options = {
    programs.az-evince.enable = lib.mkEnableOption "Enable evince.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ evince ];
    xdg.mimeApps = {
      enable = true;
      associations.added = {
        "application/pdf" = [ "org.gnome.Evince.desktop" ];
      };
      defaultApplications = {
        "application/pdf" = [ "org.gnome.Evince.desktop" ];
      };
    };
  };
}

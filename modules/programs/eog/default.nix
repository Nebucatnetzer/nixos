{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-eog;
in
{
  options = {
    programs.az-eog.enable = lib.mkEnableOption "Gnome Image Viewer";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.eog ];
    home-manager.users.${config.az-username} = {
      xdg.mimeApps = {
        enable = true;
        associations.added = {
          "image/heif" = [ "org.gnome.eog.desktop" ];
          "image/png" = [ "org.gnome.eog.desktop" ];
          "image/jpeg" = [ "org.gnome.eog.desktop" ];
        };
        defaultApplications = {
          "image/heif" = [ "org.gnome.eog.desktop" ];
          "image/png" = [ "org.gnome.eog.desktop" ];
          "image/jpeg" = [ "org.gnome.eog.desktop" ];
        };
      };
    };
  };
}

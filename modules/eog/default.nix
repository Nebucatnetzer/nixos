{ config, custom, lib, pkgs, ... }:
let
  cfg = config.programs.eog;
in
{
  options = {
    programs.eog.enable = lib.mkEnableOption "Gnome Image Viewer";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gnome.eog
    ];
    home-manager.users.${custom.username} = {
      xdg.mimeApps = {
        enable = true;
        associations.added = {
          "image/png" = [ "org.gnome.eog.desktop" ];
          "image/jpeg" = [ "org.gnome.eog.desktop" ];
        };
        defaultApplications = {
          "image/png" = [ "org.gnome.eog.desktop" ];
          "image/jpeg" = [ "org.gnome.eog.desktop" ];
        };
      };
    };
  };
}


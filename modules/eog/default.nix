{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-eog;
in
{
  options = {
    programs.az-eog.enable = lib.mkEnableOption "Gnome Image Viewer";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gnome.eog
    ];
    home-manager.users.${config.az-username} = {
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


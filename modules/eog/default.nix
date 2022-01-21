{ pkgs, ... }:
let
  username = import ../../username.nix;
in
{
  environment.systemPackages = with pkgs; [
    gnome.eog
  ];

  home-manager.users.${username} = {
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
}


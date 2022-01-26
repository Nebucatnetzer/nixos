{ pkgs, ... }:
{
  home.packages = with pkgs; [
    evince
  ];
  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "application/pdf" = [ "org.gnome.Evince.desktop" ];
    };
    defaultApplications = {
      "application/pdf" = [ "org.gnome.Evince.desktop" ];
    };
  };
}

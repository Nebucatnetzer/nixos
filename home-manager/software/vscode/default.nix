{ nixpkgs-unstable, ... }:
{
  home.packages = with nixpkgs-unstable; [
    vscode
  ];
#  xdg.mimeApps = {
#    enable = true;
#    associations.added = {
#      "application/pdf" = [ "org.gnome.Evince.desktop" ];
#    };
#    defaultApplications = {
#      "application/pdf" = [ "org.gnome.Evince.desktop" ];
#    };
#  };
}

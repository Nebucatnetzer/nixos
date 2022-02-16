{ inputs, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gnome.eog
  ];

  home-manager.users.${inputs.custom.username} = {
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


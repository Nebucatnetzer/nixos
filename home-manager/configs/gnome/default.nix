{ lib, ... }:
{
  home.file.".config/touchegg/touchegg.conf".source = ./touchegg.conf;
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      xkb-options = [ "compose:ralt" ];
    };
    "org/gnome/desktop/interface" = {
      color-scheme = "default";
    };
    "org/gnome/desktop/session" = {
      idle-delay = lib.hm.gvariant.mkUint32 300;
    };
    "org/gnome/desktop/wm/keybindings" = {
      show-desktop = [ "<Super> d" ];
      switch-applications = [ "<Super>Tab" ];
      witch-windows = [ "<Alt>Tab" ];
    };
    "org/gnome/desktop/privacy" = {
      recent-files-max-age = 30;
      remember-recent-files = true;
      remove-old-trash-files = true;
      remove-old-temp-files = true;
      old-files-age = lib.hm.gvariant.mkUint32 30;
    };
    "org/gnome/desktop/screensaver" = {
      lock-delay = "uint32 0";
      lock-enabled = true;
    };
    "/org/gnome/desktop/notifications" = {
      "show-in-lock-screen" = false;
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      home = [ "<Super>e" ];
      www = [ "<Super>w" ];
      volume-mute = [ "<Super>F1" ];
      volume-down = [ "<Super>F2" ];
      volume-up = [ "<Super>F3" ];
      play = [ "<Super>F6" ];
      previous = [ "<Super>F5" ];
      next = [ "<Super>F7" ];
    };
    "org/freedesktop/tracker/miner/files" = {
      index-recursive-directories = [
        "&DOCUMENTS"
        "&MUSIC"
        "&PICTURES"
        "&VIDEOS"
        "&DOWNLOAD"
        "/home/andreas/nextcloud/10_documents"
      ];
    };
    "org/gnome/shell/app-switcher" = {
      current-workspace-only = true;
    };
  };
}


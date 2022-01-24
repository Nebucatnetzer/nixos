{ self, pkgs, ... }:
{
  imports = [
    (import ./common { inherit self; })
    ./software/work-desktop
    ./software/autorandr
    ./software/czkawka
    ./software/dunst
    ./software/evince
    ./software/git
    ./software/keeweb
    ./software/mpv
    ./software/nitrogen
    ./software/obsidian
    ./software/pycharm
    ./software/rapid-photo-downloader
    ./software/signal
    ./software/telegram
    ./software/vim
  ];
  home.packages = with pkgs; [
    arc-theme
    calibre
    digikam
    firefox
    gimp
    inkscape
    libreoffice-fresh
    meld
    remmina
    shotwell
    unstable.vscode
    unstable.youtube-dl
  ];
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile/config.py".source = ./configs/qtile/config.py;
  home.file.".config/qtile/autostart.sh".source = ./configs/qtile/autostart.sh;
  home.file.".config/terminator".source = ./configs/terminator;

  programs.bash = {
    enable = true;
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  gtk.theme.name = "Arc-Darker";

  xsession.numlock.enable = true;
  services.network-manager-applet.enable = true;
}

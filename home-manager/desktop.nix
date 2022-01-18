{ pkgs, ... }:
{
  imports = [
    ./common
    ./software/work-desktop
    ./software/autorandr
    ./software/czkawka
    ./software/dunst
    ./software/git
    ./software/keeweb
    ./software/lorri
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
    evince
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
    shellAliases = {
      management-server = "mosh --ssh='ssh -p 22' andreas@10.7.89.106 tmux a";
    };
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  gtk.theme.name = "Arc-Darker";

  xsession.numlock.enable = true;
  services.network-manager-applet.enable = true;
}

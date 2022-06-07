{ inputs, custom, pkgs, ... }:
{
  imports = [
    ./software/ansible
    (import ./common { inherit custom inputs; })
    ./software/calibre
    ./software/czkawka
    ./software/dunst
    (import ./software/emacs { inherit custom pkgs; })
    ./software/email
    ./software/evince
    ./software/fzf
    ./software/git
    ./software/grobi
    ./software/keeweb
    ./software/libimobiledevice
    ./software/mime-apps
    ./software/mpv
    ./software/nitrogen
    ./software/obsidian
    ./software/pycharm
    ./software/rapid-photo-downloader
    ./software/signal
    ./software/starship
    ./software/telegram
    ./software/vim
    ./software/work-desktop
  ];
  home.packages = with pkgs; [
    arc-theme
    digikam
    firefox
    gimp
    inkscape
    libreoffice-fresh
    meld
    remmina
    shotwell
    vscode
    yt-dlp
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

  xsession = {
    numlock.enable = true;
  };
  services.network-manager-applet.enable = true;
}

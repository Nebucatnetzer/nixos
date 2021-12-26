{ pkgs, inputs, system, ... }:
{
  imports = [
    ./common
    ./work-desktop.nix
    ./software/ansible
    ./software/dunst
    ./software/git
    ./software/keeweb
    ./software/mpv
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
    czkawka
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
  home.file.".config/qtile".source = ./configs/qtile;
  home.file.".config/terminator".source = ./configs/terminator;
  home.file.".local/share/applications/steam.desktop".source = ./desktop/applications/steam.desktop;

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

  xsession.numlock.enable = true;
  services.network-manager-applet.enable = true;
}

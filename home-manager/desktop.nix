{ custom }: { config, pkgs, ... }:
{
  imports = [
    "${custom.inputs.self}/home-manager/software/ansible"
    (import "${custom.inputs.self}/home-manager/common" { inherit custom; })
    "${custom.inputs.self}/home-manager/software/calibre"
    "${custom.inputs.self}/home-manager/software/czkawka"
    "${custom.inputs.self}/home-manager/software/dunst"
    "${custom.inputs.self}/home-manager/software/emacs"
    "${custom.inputs.self}/home-manager/software/evince"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/grobi"
    "${custom.inputs.self}/home-manager/software/keeweb"
    "${custom.inputs.self}/home-manager/software/mime-apps"
    "${custom.inputs.self}/home-manager/software/mpv"
    "${custom.inputs.self}/home-manager/software/nitrogen"
    "${custom.inputs.self}/home-manager/software/obsidian"
    (import "${custom.inputs.self}/home-manager/software/podget" {
      downloadDir = "/home/andreas/Downloads";
    })
    "${custom.inputs.self}/home-manager/software/pycharm"
    "${custom.inputs.self}/home-manager/software/rapid-photo-downloader"
    "${custom.inputs.self}/home-manager/software/signal"
    "${custom.inputs.self}/home-manager/software/ssh"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/telegram"
    "${custom.inputs.self}/home-manager/software/vim"
    "${custom.inputs.self}/home-manager/software/work-desktop"
  ];
  home.packages = with pkgs; [
    arc-theme
    digikam
    firefox
    gimp
    inkscape
    libreoffice-fresh
    makemkv
    meld
    remmina
    shotwell
    sound-juicer
    vscode
    yt-dlp
  ];
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile/config.py".source = "${custom.inputs.self}/home-manager/configs/qtile/config.py";
  home.file.".config/qtile/autostart.sh".source = "${custom.inputs.self}/home-manager/configs/qtile/autostart.sh";
  home.file.".config/terminator".source = "${custom.inputs.self}/home-manager/configs/terminator";

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      open-port() {
        local port=$1
        sudo iptables -A INPUT -p tcp --dport $port -j ACCEPT
      }

      close-port() {
        local port=$1
        sudo iptables -D INPUT -p tcp --dport $port -j ACCEPT
      }
    '';
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  gtk.theme.name = "Arc-Darker";

  xdg.userDirs = {
    enable = true;
    pictures = "${config.home.homeDirectory}/nextcloud/20_pictures";
    createDirectories = true;
  };

  xsession = {
    numlock.enable = true;
  };
  services.network-manager-applet.enable = true;
}

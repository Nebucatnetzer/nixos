{ inputs, custom }: { pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/software/ansible"
    (import "${inputs.self}/home-manager/common" { inherit custom inputs; })
    "${inputs.self}/home-manager/software/calibre"
    "${inputs.self}/home-manager/software/czkawka"
    "${inputs.self}/home-manager/software/dunst"
    "${inputs.self}/home-manager/software/emacs"
    "${inputs.self}/home-manager/software/email"
    "${inputs.self}/home-manager/software/evince"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/grobi"
    "${inputs.self}/home-manager/software/keeweb"
    "${inputs.self}/home-manager/software/mime-apps"
    "${inputs.self}/home-manager/software/mpv"
    "${inputs.self}/home-manager/software/nitrogen"
    "${inputs.self}/home-manager/software/obsidian"
    (import "${inputs.self}/home-manager/software/podget" {
      downloadDir = "/home/andreas/Downloads";
    })
    "${inputs.self}/home-manager/software/pycharm"
    "${inputs.self}/home-manager/software/rapid-photo-downloader"
    "${inputs.self}/home-manager/software/signal"
    "${inputs.self}/home-manager/software/ssh"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/telegram"
    "${inputs.self}/home-manager/software/vim"
    "${inputs.self}/home-manager/software/work-desktop"
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
  home.file.".config/qtile/config.py".source = "${inputs.self}/home-manager/configs/qtile/config.py";
  home.file.".config/qtile/autostart.sh".source = "${inputs.self}/home-manager/configs/qtile/autostart.sh";
  home.file.".config/terminator".source = "${inputs.self}/home-manager/configs/terminator";

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

  xsession = {
    numlock.enable = true;
  };
  services.network-manager-applet.enable = true;
}

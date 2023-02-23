{ custom }: { config, pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/home-manager/common" { inherit custom; })
    "${custom.inputs.self}/home-manager/software/calibre"
    "${custom.inputs.self}/home-manager/software/czkawka"
    "${custom.inputs.self}/home-manager/software/emacs"
    "${custom.inputs.self}/home-manager/software/espanso"
    "${custom.inputs.self}/home-manager/software/evince"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/keeweb"
    "${custom.inputs.self}/home-manager/software/mime-apps"
    "${custom.inputs.self}/home-manager/software/mpv"
    "${custom.inputs.self}/home-manager/software/obsidian"
    (import "${custom.inputs.self}/home-manager/software/rapid-photo-downloader" { inherit custom; })
    "${custom.inputs.self}/home-manager/software/signal"
    "${custom.inputs.self}/home-manager/software/ssh"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/telegram"
    "${custom.inputs.self}/home-manager/software/vim"
    "${custom.inputs.self}/home-manager/software/work-desktop"
    "${custom.inputs.self}/home-manager/software/yt-dlp"
  ];
  home.packages = with pkgs; [
    arc-theme
    digikam
    freetube
    chromium
    libreoffice-fresh
    makemkv
    meld
    plexamp
    remmina
    shotwell
    sound-juicer
    unstable.tagger
  ];
  programs.git.userEmail = "andreas@zweili.ch";

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
}


{ config, inputs, nixosConfig, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
    "${inputs.self}/home-manager/software/emacs"
    "${inputs.self}/home-manager/software/espanso"
    "${inputs.self}/home-manager/software/evince"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/keeweb"
    "${inputs.self}/home-manager/software/mime-apps"
    "${inputs.self}/home-manager/software/mpv"
    "${inputs.self}/home-manager/software/nitrogen"
    "${inputs.self}/home-manager/software/obsidian"
    "${inputs.self}/home-manager/software/rapid-photo-downloader"
    "${inputs.self}/home-manager/software/signal"
    "${inputs.self}/home-manager/software/ssh"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/telegram"
    "${inputs.self}/home-manager/software/vim"
    "${inputs.self}/home-manager/software/work-desktop"
    "${inputs.self}/home-manager/software/yt-dlp"
  ];
  home = {
    username = nixosConfig.az-username;
    packages = with pkgs; [
      digikam
      docker-compose
      exercism
      freetube
      chromium
      libreoffice-fresh
      meld
      nodePackages.prettier # formatting files
      plexamp
      remmina
      shotwell
      sound-juicer
      unstable.tagger
    ];
  };
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = "${inputs.self}/home-manager/configs/flatpak/xdg-portal-add-path.sh";

  programs = {
    az-calibre.enable = true;
    az-czkawka.enable = true;
    bash = {
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
      shellAliases = {
        management-server = "mosh ${config.home.username}@10.7.89.150 -- tmux new -A -s 0";
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
      };
    };
  };

  services = {
    az-dunst.enable = true;
    az-grobi.enable = true;
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
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


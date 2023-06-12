{ config, inputs, nixosConfig, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
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
      nitrogen
      nodePackages.prettier # formatting files
      plexamp
      remmina
      shotwell
      sound-juicer
      unstable.obsidian
      unstable.tagger
    ];
  };

  # raw config files
  home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = "${inputs.self}/home-manager/configs/flatpak/xdg-portal-add-path.sh";

  programs = {
    az-alacritty.enable = true;
    az-calibre.enable = true;
    az-czkawka.enable = true;
    az-emacs.enable = true;
    az-evince.enable = true;
    az-git.enable = true;
    az-keeweb.enable = true;
    az-mpv.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-signal.enable = true;
    az-ssh.enable = true;
    az-telegram.enable = true;
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
    az-espanso.enable = true;
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
  # forcecully override the mimeapps.list
  # this is required because it isn't a file nix can easily lock
  # https://github.com/nix-community/home-manager/issues/1213
  xdg.configFile."mimeapps.list".force = true;


  xsession = {
    numlock.enable = true;
  };
  services.network-manager-applet.enable = true;
}


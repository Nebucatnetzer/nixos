{ config, inputs, nixosConfig, pkgs, system, ... }:
let
  unstable = import inputs.nixpkgs-unstable { inherit system; };
in
{
  imports = [
    "${inputs.self}/home-manager/common"
    "${inputs.self}/home-manager/software/alacritty"
    "${inputs.self}/home-manager/software/ansible"
    "${inputs.self}/home-manager/software/calibre"
    "${inputs.self}/home-manager/software/czkawka"
    (import "${inputs.self}/home-manager/software/emacs" { inherit unstable; })
    "${inputs.self}/home-manager/software/espanso"
    "${inputs.self}/home-manager/software/dunst"
    "${inputs.self}/home-manager/software/evince"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/grobi"
    "${inputs.self}/home-manager/software/keeweb"
    "${inputs.self}/home-manager/software/mime-apps"
    "${inputs.self}/home-manager/software/mpv"
    "${inputs.self}/home-manager/software/nitrogen"
    "${inputs.self}/home-manager/software/obsidian"
    (import "${inputs.self}/home-manager/software/rapid-photo-downloader" { inherit unstable; })
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
      nodejs # needed for ansible-language-server
      nodePackages.prettier # formatting files
      pulseaudio # required for volume controls in qtile
      plexamp
      remmina
      shotwell
      sound-juicer
      unstable.tagger
    ];
  };
  programs.git.userEmail = "andreas@zweili.ch";

  # raw config files
  home.file.".config/qtile/config.py".source = "${inputs.self}/home-manager/configs/qtile/config.py";
  home.file.".config/qtile/autostart.sh".source = "${inputs.self}/home-manager/configs/qtile/autostart.sh";
  home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = "${inputs.self}/home-manager/configs/flatpak/xdg-portal-add-path.sh";

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
    shellAliases = {
      management-server = "mosh ${config.home.username}@10.7.89.150 -- tmux new -A -s 0";
      work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
    };
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


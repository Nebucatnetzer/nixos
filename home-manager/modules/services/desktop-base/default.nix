{ config, lib, pkgs, ... }:
let
  cfg = config.services.az-desktop-base;
in
{
  options = {
    services.az-desktop-base.enable = lib.mkEnableOption "Base configuration for a destkop.";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      az-alacritty.enable = true;
      az-czkawka.enable = true;
      az-emacs.enable = true;
      az-evince.enable = true;
      az-keeweb.enable = true;
      az-mpv.enable = true;
      az-open-port.enable = true;
      az-signal.enable = true;
      az-ssh.enable = true;
      az-telegram.enable = true;
      bash = {
        shellAliases = {
          management-server = "mosh ${config.home.username}@10.7.89.150 -- tmux new -A -s 0";
          work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
        };
      };
    };
    home.packages = with pkgs; [
      docker-compose
      meld
      nitrogen
      libreoffice-fresh
      nodePackages.prettier # formatting files
      remmina
    ];
    # raw config files
    home.file.".config/qtile/autostart.d/xdg-portal-add-path.sh".source = ./xdg-portal-add-path.sh;


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

    services = {
      az-dunst.enable = true;
      az-espanso.enable = true;
      az-grobi.enable = true;
      network-manager-applet.enable = true;
      nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}


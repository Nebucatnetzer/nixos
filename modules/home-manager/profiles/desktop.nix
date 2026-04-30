{
  config,
  inputs,
  nixosConfig,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs unstable-pkgs; };
in
{
  imports = [
    "${inputs.self}/modules/home-manager/programs/beets"
    "${inputs.self}/modules/home-manager/programs/calibre"
    "${inputs.self}/modules/home-manager/programs/mpv"
    "${inputs.self}/modules/home-manager/programs/rapid-photo-downloader"
    "${inputs.self}/modules/home-manager/programs/signal"
    "${inputs.self}/modules/home-manager/programs/telegram"
    "${inputs.self}/modules/home-manager/programs/work-desktop"
    "${inputs.self}/modules/home-manager/services/desktop-base"
    ./management.nix
  ];
  home = {
    file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

    packages = [
      azPkgs.az-media
      azPkgs.dap-sync
      azPkgs.download-articles
      azPkgs.download-playlist
      azPkgs.download-video
      azPkgs.jdownloader
      azPkgs.toggle-keyboard
      azPkgs.video-to-mpv
      azPkgs.watch-playlist
      azPkgs.watch-random-video
      azPkgs.watch-video

      # photography packages
      unstable-pkgs.darktable
      pkgs.digikam
      pkgs.hugin

      # wine
      pkgs.bottles
      pkgs.wine-wayland
      pkgs.wineWowPackages.waylandFull

      pkgs.chromium # needed for cloud gaming and flashing zsa
      pkgs.czkawka
      pkgs.exercism
      pkgs.keepassxc
      pkgs.libreoffice-qt-fresh
      pkgs.meld
      pkgs.plex-desktop
      pkgs.remmina
      pkgs.tagger
    ];
  };

  programs = {
    bash = {
      shellAliases = {
        management-server = "mosh ${config.home.username}@${nixosConfig.az-hosts.gwyn.wgIp} -- tmux new -A -s 0";
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
      };
    };
    tmux.mouse = true;
  };

}

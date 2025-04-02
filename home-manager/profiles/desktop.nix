{
  config,
  inputs,
  pkgs,
  ...
}:
let
  az-media = pkgs.writeShellScriptBin "az-media" ''
    videos="videos"
    directory="''${1:-videos}"
    for i in $(seq 1 4);
    do
        mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
    done
  '';
  dap-sync = pkgs.callPackage "${inputs.self}/pkgs/dap-sync" { };
  freetube = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.freetube;
  send-to-kindle = pkgs.callPackage "${inputs.self}/pkgs/send-to-kindle" { };
  unstable-pkgs = import inputs.nixpkgs-unstable {
    system = pkgs.system;
    config = {
      allowUnfree = true;
    };
  };
in
{
  imports = [ ./management.nix ];
  home = {
    file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

    packages = [
      pkgs.chromium # needed for cloud gaming and flashing zsa
      pkgs.digikam
      pkgs.exercism
      pkgs.libreoffice-fresh
      pkgs.meld
      pkgs.nitrogen
      pkgs.plexamp
      pkgs.remmina
      pkgs.sound-juicer
      pkgs.tagger
      az-media
      dap-sync
      freetube
      send-to-kindle
      unstable-pkgs.vscode
    ];
  };

  programs = {
    az-alacritty.enable = true;
    az-beets.enable = true;
    az-calibre.enable = true;
    az-mpv.enable = true;
    az-rapid-photo-downloader.enable = true;
    az-signal.enable = true;
    az-telegram.enable = true;
    az-work-desktop.enable = true;
    bash = {
      shellAliases = {
        management-server = "mosh ${config.home.username}@10.7.89.153 -- tmux new -A -s 0";
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
      };
    };
  };

}

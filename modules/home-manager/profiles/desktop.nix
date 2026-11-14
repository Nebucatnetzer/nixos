{
  config,
  inputs,
  lib,
  nixosConfig,
  pkgs,
  ...
}:
{
  imports = [
    "${inputs.self}/modules/home-manager/programs/beets"
    "${inputs.self}/modules/home-manager/programs/darktable"
    "${inputs.self}/modules/home-manager/programs/digikam"
    "${inputs.self}/modules/home-manager/programs/mpv"
    "${inputs.self}/modules/home-manager/programs/pi"
    "${inputs.self}/modules/home-manager/programs/rapid-photo-downloader"
    "${inputs.self}/modules/home-manager/services/desktop-base"
    ./management.nix
  ];
  home = {
    file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";
  };

  programs = {
    bash = {
      shellAliases = {
        fenoglio = "mosh ${config.home.username}@${nixosConfig.az-hosts.fenoglio.wgIp} -- tmux new -A -s 0";
        # gwyn is not intented to be worked on so we connect directly via SSH.
        gwyn = "ssh ${config.home.username}@${nixosConfig.az-hosts.gwyn.wgIp}";
      };
    };
    emacs.package = lib.mkForce pkgs.emacs-pgtk;
  };

}

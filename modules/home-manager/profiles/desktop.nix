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
    "${inputs.self}/modules/home-manager/programs/art"
    "${inputs.self}/modules/home-manager/programs/beets"
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
        gwyn = "mosh ${config.home.username}@${nixosConfig.az-hosts.gwyn.wgIp} -- tmux new -A -s 0";
      };
    };
    emacs.package = lib.mkForce pkgs.emacs-pgtk;
    # Must come after the unbind in the tmux module. Copy-mode survives repeated selections.
    tmux.extraConfig = lib.mkAfter ''
      bind-key -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-selection
    '';
  };

}

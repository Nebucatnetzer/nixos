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
    "${inputs.self}/modules/home-manager/programs/mpv"
    "${inputs.self}/modules/home-manager/programs/pi"
    "${inputs.self}/modules/home-manager/programs/rapid-photo-downloader"
    "${inputs.self}/modules/home-manager/services/desktop-base"
    ./management.nix
  ];
  home = {
    file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";
    file.".config/darktable/luarc".text = ''
      package.path = package.path .. ";${inputs.darktable-lua-scripts}/?.lua"
    '';
  };

  programs = {
    bash = {
      shellAliases = {
        fenoglio = "mosh ${config.home.username}@${nixosConfig.az-hosts.fenoglio.wgIp} -- tmux new -A -s 0";
        gwyn = "mosh ${config.home.username}@${nixosConfig.az-hosts.gwyn.wgIp} -- tmux new -A -s 0";
      };
    };
    emacs.package = lib.mkForce pkgs.emacs-pgtk;
    tmux.mouse = true;
  };

}

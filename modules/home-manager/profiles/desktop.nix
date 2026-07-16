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
        management-server = "mosh ${config.home.username}@${nixosConfig.az-hosts.gwyn.wgIp} -- tmux new -A -s 0";
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
      };
    };
    emacs.package = lib.mkForce pkgs.emacs-pgtk;
    tmux.mouse = true;
  };

}

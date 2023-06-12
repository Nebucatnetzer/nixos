{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-work-desktop;
in
{
  options = {
    programs.az-work-desktop.enable = lib.mkEnableOption "Applications and config required for work.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      dbeaver
      vagrant
    ];
  };
}

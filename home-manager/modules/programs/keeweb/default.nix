{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-keeweb;
in
{
  options = {
    programs.az-keeweb.enable = lib.mkEnableOption "Enable keeweb.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      keeweb
    ];

    home.file.".config/qtile/autostart.d/keeweb.sh".source = ./keeweb.sh;
  };
}

{ config, custom, lib, pkgs, ... }:
let
  cfg = config.programs.az-makemkv;
in
{
  options = {
    programs.az-makemkv.enable = lib.mkEnableOption "MakeMKV";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${custom.username} = {
      home.packages = with pkgs; [
        makemkv
      ];
    };
    boot.kernelModules = [ "sg" ];
  };
}

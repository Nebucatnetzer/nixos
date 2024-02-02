{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-makemkv;
in
{
  options = {
    programs.az-makemkv.enable = lib.mkEnableOption "MakeMKV";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.az-username} = {
      home.packages = with pkgs; [ makemkv ];
    };
    boot.kernelModules = [ "sg" ];
  };
}

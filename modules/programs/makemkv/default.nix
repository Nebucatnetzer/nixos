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
    # https://forum.makemkv.com/forum/viewtopic.php?t=1053
    home-manager.users.${config.az-username} = {
      home.packages = [ pkgs.makemkv ];
    };
    boot.kernelModules = [ "sg" ];
  };
}

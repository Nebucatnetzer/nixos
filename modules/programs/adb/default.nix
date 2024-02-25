{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-adb;
in
{
  options = {
    programs.az-adb.enable = lib.mkEnableOption "Enable and configure ADB";
  };

  config = lib.mkIf cfg.enable {
    programs.adb.enable = true;
    users.users."${config.az-username}".extraGroups = [ "adbusers" ];
  };
}

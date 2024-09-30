{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-adb;
  sidequest = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.sidequest;
in
{
  options = {
    programs.az-adb.enable = lib.mkEnableOption "Enable and configure ADB";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ sidequest ];
    programs.adb.enable = true;
    users.users."${config.az-username}".extraGroups = [ "adbusers" ];
  };
}

{ config, ... }:
{
  programs.adb.enable = true;
  users.users."${config.az-username}".extraGroups = [ "adbusers" ];
}

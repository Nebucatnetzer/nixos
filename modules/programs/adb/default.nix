{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.android-tools ];
  users.users."${config.az-username}".extraGroups = [ "adbusers" ];
}

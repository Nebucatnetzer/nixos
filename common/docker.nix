{ config, pkgs, ... }:
{
  virtualisation.docker.enable = true;
  users.users.andreas.extraGroups = [ "docker" ];

}

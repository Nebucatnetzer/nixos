{ config, pkgs, ... }:
{
  virtualisation.docker =
    {
      enable = true;
      autoPrune.enable = true;
    };
  users.users.andreas.extraGroups = [ "docker" ];

}

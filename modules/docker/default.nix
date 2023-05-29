{ config, pkgs, ... }:
{
  virtualisation.docker =
    {
      enable = true;
      autoPrune.enable = true;
    };
  users.users.${config.az-username}.extraGroups = [ "docker" ];
  environment.systemPackages = with pkgs; [
    docker-compose
    lazydocker
  ];
}

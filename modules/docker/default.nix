{ inputs, custom, pkgs, ... }:
{
  virtualisation.docker =
    {
      enable = true;
      autoPrune.enable = true;
    };
  users.users.${custom.username}.extraGroups = [ "docker" ];
  environment.systemPackages = with pkgs; [
    docker-compose
    lazydocker
  ];
}

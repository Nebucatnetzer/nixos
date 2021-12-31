{ pkgs, ... }:
let
  username = import ../username.nix;
in
{
  virtualisation.docker =
    {
      enable = true;
      autoPrune.enable = true;
    };
  users.users.${username}.extraGroups = [ "docker" ];
  environment.systemPackages = with pkgs; [
    docker-compose
    lazydocker
  ];


}

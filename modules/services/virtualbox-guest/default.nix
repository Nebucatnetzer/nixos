{ config, ... }:
{
  virtualisation.virtualbox.guest = {
    clipboard = true;
    draganddrop = true;
    enable = true;
  };
  users.users.${config.az-username} = {
    extraGroups = [ "vboxsf" ];
  };
}

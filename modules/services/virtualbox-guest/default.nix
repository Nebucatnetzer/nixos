{ config, lib, ... }:
let
  cfg = config.services.az-virtualbox-guest;
in
{
  options = {
    services.az-virtualbox-guest.enable = lib.mkEnableOption "Enable virtualbox services";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.virtualbox.guest = {
      clipboard = true;
      draganddrop = true;
      enable = true;
    };
    users.users.${config.az-username} = {
      extraGroups = [ "vboxsf" ];
    };
  };
}

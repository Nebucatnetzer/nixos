{ config, lib, ... }:
let
  cfg = config.services.az-virtualbox-guest;
in
{
  options = {
    services.az-virtualbox-guest.enable = lib.mkEnableOption "Enable virtualbox services";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.virtualbox.guest.enable = true;
    virtualisation.virtualbox.guest.x11 = true;
    users.users.${config.az-username} = {
      extraGroups = [ "vboxsf" ];
    };
  };
}

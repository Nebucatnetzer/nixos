{ config, lib, pkgs, ... }:
let cfg = config.services.az-docker;
in {
  options = { services.az-docker.enable = lib.mkEnableOption "Enable Docker"; };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };
    users.users.${config.az-username}.extraGroups = [ "docker" ];
    environment.systemPackages = with pkgs; [ lazydocker ];
  };
}

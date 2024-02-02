{ config, lib, ... }:
let
  cfg = config.services.az-pipewire;
in
{
  options = {
    services.az-pipewire.enable = lib.mkEnableOption "Enable pipewire";
  };

  config = lib.mkIf cfg.enable {
    hardware.pulseaudio.enable = lib.mkForce false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    environment.etc.pipwire-config = {
      enable = true;
      text = ''
        {
          "context.exec": [
            {
              "args": "load-module module-switch-on-connect",
              "path": "pactl"
            }
          ]
        }
      '';
      target = "pipewire.conf.d/auto-switch-audio.conf";
    };
    home-manager.users.${config.az-username} = {
      services.easyeffects.enable = true;
    };
  };
}

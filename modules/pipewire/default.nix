{ config, lib, ... }:
{
  hardware.pulseaudio.enable = lib.mkForce false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    config.pipewire = {
      "context.exec" = [{
        path = "pactl";
        args = "load-module module-switch-on-connect";
      }];
    };
  };
  home-manager.users.${config.az-username} = {
    services.easyeffects.enable = true;
  };
}

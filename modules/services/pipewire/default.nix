{ lib, ... }:
{
  services.pulseaudio.enable = lib.mkForce false;
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
}

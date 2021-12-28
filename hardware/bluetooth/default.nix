{ pkgs, ... }:

{
  # Blueooth support in general
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  # Blueman applet
  services.blueman.enable = true;

  # aptx/LDAC support
  hardware.pulseaudio = {
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };

  systemd.user.services.blueman-applet = {
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
  };

}


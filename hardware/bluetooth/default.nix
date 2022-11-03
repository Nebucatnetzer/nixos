{ ... }:

{
  # Blueooth support in general
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  # Blueman applet
  services.blueman.enable = true;

  systemd.user.services.blueman-applet = {
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
  };

}


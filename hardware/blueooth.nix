# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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
    description = "Blueman Applet";
    after = [ "multi-user.target" ];
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${blueman}/bin/blueman-applet";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      KillMode = "process";
      Restart = "on-failure";
    };
  };

}


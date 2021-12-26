# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./docker.nix
    ./restic
    ./droidcam
    ./xonsh
  ];

  networking = {
    networkmanager.enable = true;
  };

  services = {
    autorandr.enable = true;
    gvfs.enable = true;
    printing.enable = true;
    redshift = {
      enable = true;
    };
    fwupd.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = true;
    displayManager.defaultSession = "none+qtile";
    windowManager.qtile.enable = true;
    layout = "us";
    xkbOptions = "compose:ralt";
    libinput.enable = true;
  };

  fonts.fonts = with pkgs; [
    source-code-pro
  ];

  # Enable keyring
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.gnome.gnome-keyring.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # enable steam
  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;

  # enable lockscreen
  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.i3lock} -c 000000";
  };

  # Enable dconf to be able to save Nautilus settings
  programs.dconf.enable = true;

  environment.variables = {
    ZWEILI_ENVIRONMENT = "desktop";
  };

  environment.systemPackages = with pkgs; [
    # what I consider to be system packages
    brightnessctl
    i3lock
    lm_sensors
    lxappearance
    gnome.eog
    gnome.nautilus
    networkmanager-openvpn
    nitrogen
    pavucontrol
    rofi
    source-code-pro
  ];
}


{ inputs, custom, pkgs, ... }:
{
  networking = {
    networkmanager.enable = true;
  };

  services = {
    gvfs.enable = true;
    printing.enable = true;
    picom = {
      enable = true;
    };
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
    useGlamor = true;
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

  # Enable dconf to be able to save Nautilus settings
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    # what I consider to be system packages
    appimage-run
    brightnessctl
    lm_sensors
    lxappearance
    gnome.nautilus
    gnome.gnome-screenshot
    networkmanager-openvpn
    nitrogen
    pavucontrol
    rofi
    source-code-pro
    terminator
  ];
  environment.shellAliases = {
    management-server = "mosh ${custom.username}@10.7.89.150 -- tmux new -A -s 0";
  };
}


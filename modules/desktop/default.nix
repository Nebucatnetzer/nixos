{ custom }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/droidcam" { inherit custom; })
    (import "${custom.inputs.self}/modules/email" { inherit custom; })
    (import "${custom.inputs.self}/modules/eog" { inherit custom; })
    (import "${custom.inputs.self}/modules/espanso" { inherit custom; })
    "${custom.inputs.self}/modules/hunspell"
    (import "${custom.inputs.self}/modules/libimobiledevice" { inherit custom; })
    (import "${custom.inputs.self}/modules/nix-direnv" { inherit custom; })
    (import "${custom.inputs.self}/modules/pipewire" { inherit custom; })
    "${custom.inputs.self}/modules/scripts"
    "${custom.inputs.self}/modules/tmux"
  ];
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
  };

  fonts.fonts = with pkgs; [
    source-code-pro
  ];

  # Enable keyring
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.gnome.gnome-keyring.enable = true;

  # Enable sound.
  sound.enable = true;

  # Enable dconf to be able to save Nautilus settings
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    # what I consider to be system packages
    appimage-run
    brightnessctl
    lm_sensors
    lxappearance
    gnome.gnome-screenshot
    gnome.nautilus
    networkmanager-openvpn
    nitrogen
    p7zip
    pavucontrol
    quickemu
    rofi
    source-code-pro
    terminator
    unrar
  ];
  environment.shellAliases = {
    management-server = "mosh ${custom.username}@10.7.89.150 -- tmux new -A -s 0";
  };
}


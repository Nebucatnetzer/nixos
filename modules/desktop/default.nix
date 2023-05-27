{ custom }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/email" { inherit custom; })
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
      vSync = true;
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

  # Gnome Image Viewer
  programs.eog.enable = true;

  # Enable Flatpack
  services.flatpak.enable = true;
  xdg = {
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
    mime = {
      addedAssociations = {
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "x-scheme-handler/chrome" = "firefox.desktop";
        "text/html" = "firefox.desktop";
        "application/x-extension-htm" = "firefox.desktop";
        "application/x-extension-html" = "firefox.desktop";
        "application/x-extension-shtml" = "firefox.desktop";
        "application/xhtml+xml" = "firefox.desktop";
        "application/x-extension-xhtml" = "firefox.desktop";
        "application/x-extension-xht" = "firefox.desktop";
        "application/x-www-browser" = "firefox.desktop";
        "x-www-browser" = "firefox.desktop";
        "x-scheme-handler/webcal" = "firefox.desktop";
      };
      defaultApplications = {
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "x-scheme-handler/chrome" = "firefox.desktop";
        "text/html" = "firefox.desktop";
        "application/x-extension-htm" = "firefox.desktop";
        "application/x-extension-html" = "firefox.desktop";
        "application/x-extension-shtml" = "firefox.desktop";
        "application/xhtml+xml" = "firefox.desktop";
        "application/x-extension-xhtml" = "firefox.desktop";
        "application/x-extension-xht" = "firefox.desktop";
        "application/x-www-browser" = "firefox.desktop";
        "x-www-browser" = "firefox.desktop";
        "x-scheme-handler/webcal" = "firefox.desktop";
      };
    };
  };
  environment = {
    systemPackages = with pkgs; [
      # what I consider to be system packages
      alacritty
      appimage-run
      brightnessctl
      firefox
      lm_sensors
      lxappearance
      gnome.file-roller
      gnome.gnome-screenshot
      gnome.nautilus
      networkmanager-openvpn
      nitrogen
      p7zip
      pavucontrol
      quickemu
      rofi
      source-code-pro
      unrar
    ];
    variables = {
      WINIT_X11_SCALE_FACTOR = "1";
    };
    sessionVariables = {
      DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
    };
  };
}

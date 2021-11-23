# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./home-manager.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [ 22 ];
    # firewall.allowedUDPPorts = [ ... ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  services = {
    openssh.enable = true;
    autorandr.enable = true;
    printing.enable = true;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.andreas = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
  };

  # allow non-free packages
  nixpkgs.config.allowUnfree = true;

  # enable garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # enable steam
  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;

  # enable lockscreen

  programs.xss-lock = {
    enable = true;
    lockerCommand = "i3lock -c 000000";
  };

  environment.shellAliases = {
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    rebuild = "sudo nixos-rebuild switch";
  };

  environment.variables = {
    EDITOR = "vim";
    HIGHLIGHT_STYLE = "solarized-light";
    ZWEILI_ENVIRONMENT = "desktop";
  };

  environment.systemPackages = with pkgs; [
    arc-theme
    celluloid
    firefox
    git
    highlight
    htop
    i3lock
    killall
    libreoffice-fresh
    lxappearance
    ncdu
    nitrogen
    pavucontrol
    ranger
    rofi
    source-code-pro
    terminator
    tree
    vim
    wget
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}


# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "none+qtile";
  services.xserver.windowManager = {
    qtile.enable = true;
  };
  services.autorandr.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "compose:ralt";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable keyring
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.gnome.gnome-keyring.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.andreas = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # allow non-free packages
  nixpkgs.config.allowUnfree = true;

  # enable steam
  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;

  environment.shellAliases = {
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
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
    ncdu
    nitrogen
    ranger
    rofi
    source-code-pro
    terminator
    vim
    wget
  ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}


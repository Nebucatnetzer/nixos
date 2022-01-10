{ pkgs, ... }:
let
  username = import ../../username.nix;
in
{
  imports = [
    ../../modules/cli
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # The rough location
  location = {
    latitude = 46.948;
    longitude = 7.447;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  networking = {
    firewall.allowedTCPPorts = [ 22 ];
    # firewall.allowedUDPPorts = [ ... ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  programs.mosh.enable = true;
  services = {
    openssh.enable = true;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${username} = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
  };

  # allow non-free packages
  nixpkgs.config.allowUnfree = true;

  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''experimental-features = nix-command flakes'';

    # enable garbage collection
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };
  };


  environment.shellAliases = {
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    rebuild = "sudo nixos-rebuild -j auto switch";
  };

  environment.variables = {
    EDITOR = "vim";
    HIGHLIGHT_STYLE = "solarized-light";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = import ../../version.nix;

}


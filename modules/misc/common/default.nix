{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  nixPath = "/etc/nixPath";
  system = pkgs.system;
in
{
  age.identityPaths = [ "/home/${config.az-username}/.ssh/id_rsa" ];
  # The rough location
  location = {
    latitude = 46.948;
    longitude = 7.447;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";
  services.timesyncd.enable = lib.mkForce true;

  services.nscd.enableNsncd = true;
  networking = {
    domain = "2li.local";
    enableIPv6 = false;
    firewall = {
      allowPing = true;
      allowedTCPPorts = [ 22 ];
    };
    timeServers = [
      "10.7.89.1"
      "ch.pool.ntp.org"
    ];
  };

  hardware = {
    enableRedistributableFirmware = true;
  };

  # required in order to have apropos and whatis working
  programs.mosh.enable = true;
  programs.ssh.startAgent = true;
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
    extraConfig = ''
      AllowTcpForwarding yes
      X11Forwarding no
      AllowAgentForwarding no
      AllowStreamLocalForwarding no
      AuthenticationMethods publickey
    '';
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_NUMERIC = "de_CH.UTF-8";
      LC_TIME = "en_IE.UTF-8";
      LC_MONETARY = "de_CH.UTF-8";
      LC_PAPER = "en_IE.UTF-8";
      LC_TELEPHONE = "de_CH.UTF-8";
      LC_MEASUREMENT = "en_IE.UTF-8";
    };
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  environment = {
    variables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
    shellAliases = {
      btm = "btm --color default-light";
      sudo = "sudo ";
    };
    systemPackages = [
      inputs.agenix.packages.${system}.default
      pkgs.bottom
      pkgs.highlight
      pkgs.killall
      pkgs.man-pages
      pkgs.ncdu
      pkgs.nmon
      pkgs.pciutils # lspci
      pkgs.usbutils # lsusb
      pkgs.tree
      pkgs.unzip
      pkgs.vim
      pkgs.wget
    ];
  };
  # Disable the root user
  users.users.root.hashedPassword = "!";
  # Define a user account. Don't forget to set a password with `passwd`.
  users.users.${config.az-username} = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn"

      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDxBun0BYQaz/WjiP+g5+Hs9/JZnWAuLTpTVYgx/9shJwKS5Zu9K3I115DYOro/lpu0AMeeJca5We2AICcxYcM0lIZvsJqfOnFOHFjgmHxHc6IuzrUPM7msoLneF5lxfJ8ko2/LcFq8EtGlzjkllRpFpp2FtxDviD1lr4mJda4cOuQES4ujH3HP5Shpwa96oqnDENWCL+XPFe+Ur+88tuKTQ2MIX5Iqhs2sMIwsMI1o8HjBi4sMd+kd7qb232XcwWTlP3iIWvq/0D3OxZ6J6uSNyC4UCl781lupLOKrC6ml58RUrYP8nrF0a53+i0hgLuDiCWhj0vkY7W9nJW1no425 andreas@python"

      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHcFlnVRZEjvblLMbQX0W644Rf6oRz9ibUv1uHIZyqNQ andreas@ipad"
    ];
  };
  systemd.tmpfiles.rules = [ "L+ ${nixPath} - - - - ${pkgs.path}" ];
  nix = {
    daemonCPUSchedPolicy = "idle";
    nixPath = [ "nixpkgs=${nixPath}" ];
    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      nix-config.flake = inputs.self;
    };

    package = pkgs.nix;
    settings = {
      warn-dirty = false;
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
        "repl-flake"
      ];
      trusted-users = [
        "root"
        "@wheel"
      ];
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
        "https://cache.zweili.org/prod"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "prod:46pIZhqoueg1P4IPp8ciArCUgSXWJZAq63CwLTQN/uA="
      ];
      min-free = 1000000000;
      max-free = 10000000000;
      connect-timeout = 5;
      fallback = true;
    };
    # enable garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
      persistent = true;
    };
    optimise = {
      automatic = true;
      dates = [ "17:00" ];
    };
  };

  security.sudo = {
    wheelNeedsPassword = false;
  };

  system.activationScripts.diff = {
    supportsDryActivation = true;
    text = ''
      if [[ -e /run/current-system ]]; then
         ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
      fi
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11";
}

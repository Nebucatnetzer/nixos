{ custom }: { pkgs, ... }:
{
  imports = [
    "${custom.inputs.self}/modules/cli"
  ];

  # The rough location
  location = {
    latitude = 46.948;
    longitude = 7.447;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";
  services.timesyncd.enable = true;

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

  programs.mosh.enable = true;
  programs.ssh.startAgent = true;
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
    extraConfig = ''
      AllowTcpForwarding yes
      X11Forwarding no
      AllowAgentForwarding no
      AllowStreamLocalForwarding no
      AuthenticationMethods publickey
    '';
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Disable the root user
  users.users.root.hashedPassword = "!";
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${custom.username} = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn"

      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDxBun0BYQaz/WjiP+g5+Hs9/JZnWAuLTpTVYgx/9shJwKS5Zu9K3I115DYOro/lpu0AMeeJca5We2AICcxYcM0lIZvsJqfOnFOHFjgmHxHc6IuzrUPM7msoLneF5lxfJ8ko2/LcFq8EtGlzjkllRpFpp2FtxDviD1lr4mJda4cOuQES4ujH3HP5Shpwa96oqnDENWCL+XPFe+Ur+88tuKTQ2MIX5Iqhs2sMIwsMI1o8HjBi4sMd+kd7qb232XcwWTlP3iIWvq/0D3OxZ6J6uSNyC4UCl781lupLOKrC6ml58RUrYP8nrF0a53+i0hgLuDiCWhj0vkY7W9nJW1no425 andreas@python"

      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDg03JFE6TzMT3mZw0OJtcBwUHCB+3AHwtS981jEd/6wFQPoFrmR48yu5RWh2ikjy8axIIVRz8pX50GNRXzGLuqI8nS9Kb5CSKs4uPE7OxSbZqO+qeTiuZ00QXudUosu5w5mJ+mRYlBZw6nrLOIZjgXbfMOsRtZFWVHWr011DO/ROXYQrG5xCG8wVJgE08R4njPlwb9EiYJD9CATttX3ryejhzYEloLg+ucbfcvs85hyyuCryIB7bIyCPim4ys6/GaTr2O2jAUXsE3Vvdiybg0UAuV0t94OQvtyHAs+yvIawt4LV7WBPYFkNuZ1ahG23hZb2IcU2YX9XvAQc2NOMEDJy76WBoXHlFa4LUaRFeVkv71xvRnWBlE2zSTiA/G+1qqkD7SN6I/MV4fMxF2mpbsfoGODhqrrmpvLNQoqxgWS1kvA4g94zLFXU+8I2Oy/kRIi9rGIDESHpuImdjXfYdpRfLyLRcDhxK2BIawerDJJ4iTFRHOSXWRlEiLdWDm2WFpRQWKkJ/lMu16n78hJfc1Ga/s/70yJi2GOcQN/nvETjrNG82gyxohNAOoMMkZP2AeC0mrPRi3dEqzc4ZUq4lcPAS/WjpQC9qNDxhQJXHmreSHG+qdEJaSQLlzFEwtVPYTFh8C/l/goVI3/moRlG3Cv+FQjAulVx/xck680PHN/GQ== flunder@blink"
    ];
  };

  # allow non-free packages
  nixpkgs.config.allowUnfree = true;

  nix = {
    nixPath = [ "nixpkgs=${custom.inputs.nixpkgs}" ];
    registry = {
      nixpkgs.flake = custom.inputs.nixpkgs;
      nix-config.flake = custom.inputs.self;
    };

    package = pkgs.nix;
    settings = {
      warn-dirty = false;
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ "root" "@wheel" ];
      min-free = 1000000000;
      max-free = 10000000000;
      connect-timeout = 5;
      fallback = true;
    };
    # enable garbage collection
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 30d";
    };
  };

  environment.variables = {
    EDITOR = "vim";
    HIGHLIGHT_STYLE = "solarized-light";
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  security.sudo = {
    extraRules = [
      {
        users = [ "andreas" ];
        commands = [
          {
            command = "${pkgs.nixos-rebuild}/bin/nixos-rebuild -j auto switch";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/nixos-rebuild";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/nix-env";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = custom.version;
}


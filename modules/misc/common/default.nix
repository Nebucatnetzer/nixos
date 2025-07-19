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
  GiB = 1024 * 1024 * 1024;
in
{
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
    block.scheduler = {
      "mmcblk[0-9]*" = "bfq";
      "nvme[0-9]*" = "kyber";
    };
  };
  # Enable scheduler extensions
  services.scx.enable = true;

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
      btm = "btm --theme nord-light";
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

      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOvF2GHDFkDkbsnXBqRVxD2LwlKdbJsjkAlovtLn1XlmuRHZhgVzbdZp/Ql7mMpeZthLn6MEy+k2r48By2RbMaQ= user@ipad"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCvOF0gndAvjp5CfcsmvjnHkg68ECQwYPGKWrOhZ+QmFFiD4n/zCZw5y/ADDOXmpqV2BCqCszH9chsVdGTSQPrCz5bdTLCOIlP7n0JFS2IIo8EEh8yqz9E9Ug7oOJauyUXeJ3sSw66r6eBT9KoM7THPNGQqBtk5ssUJII0VTEgLDfgHv7UNRhBA5YKCWSrVnoA2dQ/MWGMMcLxIX0clS6dAc9+pN1maeq8buDEXN/phLVjDC9WEfiB0xKqw8+L4GJoNYvjjaAnjaSAEkUV2lo0j3v0DZL8Jt1SbXhFXYqdNkF+wBwc718x3MI30W18RC1Z1BG9atzjTS/rXRXX9azqY+HqMVxoFqPcT9RYE/yzNlNWIUUU13vpiMrMYk+S5fKg0Kf+O30IWwh9PEX/PMo32LyP98+mlKRbAlHZ1DVRSSwN/R5ftZYX45Oy5wa9O2mFL8cKOZ0U6NPASeYbDJ/QS+kjGnaNYK15/utgjZgiSSpP8HytjAnUKgWHvAXAxs3B/ABttm5qawmbMGCuqjVrRay5oY6ZubLBCKS1k8EwCNU57f9F9On7Ncybl+lYOv4j0Cu+PjwhUTDR9IEwoVa4pqArjDD4p19xoUXwDnFy1uP5LAcg5xxd/p/FRRI6ucLy6vmZgimgi76X2yF5oJL3O14Q6Zz/08FDLMVIGZwIF7w== ShellFish@iPad-27062024
"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCuUIgSKpAub1YEyQUwf884amhDDXokbG5zXY9JKCEbvTjyL9qp3rflqLM1WPVZ6JpC5XEAu29F4wrYvsGvBa5b5B5XtLcTY2wLYTnMQfVKCf1DSNyFYxCdOlHvoWPBIZDPLIThHnLTLj0vhHpd55Eiv1HuVH7JzmS9PIcd2uuo/uZQFLJWkrJZe4g43MP7TwL1Dg8kuw10xaxxKxlzAtIPWJ9G91BxAhIqmXgwF9VK91qH9z6XNDc4RGcFSUSKTp6Hapbkg2HYXtzvq/awV7F0MRIM2n+5PlQ9YVkP068iEYZpxKU5JK9XhSKdaHAAHU3CDmixhwQ6JmY+AV771Yo7j5+sCMYsyR0MWfY65+pJoSCot/yFLCF7mi+HRDSu2k8wCp3HCpNKUt3DsNGuYxz0lPaSTY9watF35NI1Mybg9EQfvUsxUMB1qD3jL6ihcc4mSezCsDKOJ2FeqJUHMG/IBBwkwFeD499hAKb+9Qyd4uuIoCsyhcIELeZQ0pDOn0TFXgPScCDrC0JxKle/VISlSfHSI0NPewA1djOSGB8ZcB8JiyVffdUuyRCKeKJUomzurzf1u8DiJ26C+KBmJwm8SrYqklBJiUjAC8kD9c1EIF0rsJK5MtSGXmw5m4hwc5kadTwCK9DToUBrAIruO0Q+gZA8+8dWS+J02vWckIIK4Q== ShellFish@iPhone-27062024
"
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
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      connect-timeout = 5;
      download-buffer-size = 134217728; # 128 MB
      fallback = true;
      min-free = 1 * GiB;
      max-free = 10 * GiB;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "zweili-cache:XIIFsRHcnocqbX47mpNXtfio/iUvTQ5cZYQS5bnz7oU="
      ];
      trusted-users = [
        "root"
        "@wheel"
      ];
      warn-dirty = false;
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
  nixpkgs.config.allowUnfree = true;

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

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = {
    inherit inputs system;
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11";

  # disable printing {
  # https://discourse.nixos.org/t/newly-announced-vulnerabilities-in-cups/52771
  services.printing.browsed.enable = false;
  ## optional
  services.avahi.enable = false;
  services.printing.enable = false;
  ## These options cause Avahi to be enabled, so either explicitly disable them or you may need to use `mkForce` above.
  services.pulseaudio.zeroconf.publish.enable = false;
  services.pulseaudio.zeroconf.discovery.enable = false;
  services.shairport-sync.enable = false;
  ## This option causes printing to be enabled, so either explicitly disable it or you may need to use `mkForce` above.
  services.printing.cups-pdf.enable = false;
  # }
}

{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self
    , agenix
    , nix-alien
    , nixpkgs
    , nixpkgs-unstable
    , nixos-hardware
    , home-manager
    }:
    let
      custom = import ./custom;
      mkComputer = import "${inputs.self}/lib/mk_computer.nix";
      mkRaspi = import "${inputs.self}/lib/mk_raspi.nix";
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer {
          hostname = "gwyn";
          home-module = "desktop";
          inherit custom inputs;
        };
        desktop-vm = mkComputer {
          hostname = "desktop-vm";
          home-module = "desktop";
          inherit custom inputs;
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
          home-module = "desktop";
          inherit custom inputs;
        };
        # Servers
        git = mkComputer {
          hostname = "git";
          inherit custom inputs;
        };
        loki-test = mkRaspi {
          hostname = "loki-test";
          inherit custom inputs;
        };
        mail = mkComputer {
          hostname = "mail";
          inherit custom inputs;
        };
        nextcloud = mkComputer {
          hostname = "nextcloud";
          inherit custom inputs;
        };
        pihole = mkComputer {
          hostname = "pihole";
          inherit custom inputs;
        };
        plex = mkComputer {
          hostname = "plex";
          inherit custom inputs;
        };
        proxy = mkRaspi {
          hostname = "proxy";
          inherit custom inputs;
        };
        raspi-test = mkRaspi {
          hostname = "raspi-test";
          home-module = "management";
          inherit custom inputs;
        };
        restic-server = mkComputer {
          hostname = "restic-server";
          inherit custom inputs;
        };
        ttrss = mkComputer {
          hostname = "ttrss";
          inherit custom inputs;
        };
      };
      homeConfigurations = {
        "${custom.username}@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          configuration = import "${inputs.self}/home-manager/work-wsl.nix";
          system = "x86_64-linux";
          username = custom.username;
          homeDirectory = "/home/${custom.username}";
          extraSpecialArgs = {
            inherit custom inputs;
          };
        };
      };
    };
}

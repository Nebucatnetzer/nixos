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
      mkComputer = import "${self}/lib/mk_computer.nix";
      mkRaspi = import "${self}/lib/mk_raspi.nix";

      system = custom.system;
      username = custom.username;

      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          system = custom.system;
          config.allowUnfree = true;
        };
      };

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
        overlays = [
          overlay-unstable
          inputs.nix-alien.overlay
        ];
      };
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer {
          hostname = "gwyn";
          home-module = "desktop";
          inherit custom pkgs inputs self;
        };
        nixos-vm = mkComputer {
          hostname = "desktop-vm";
          home-module = "desktop";
          inherit custom pkgs inputs self;
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
          home-module = "desktop";
          inherit custom pkgs inputs self;
        };
        # Servers
        git = mkComputer {
          hostname = "git";
          inherit custom pkgs inputs self;
        };
        jdownloader = mkComputer {
          hostname = "jdownloader";
          inherit custom pkgs inputs self;
        };
        mail = mkComputer {
          hostname = "mail";
          inherit custom pkgs inputs self;
        };
        nextcloud = mkComputer {
          hostname = "nextcloud";
          inherit custom pkgs inputs self;
        };
        nixos-management = mkComputer {
          hostname = "nixos-management";
          home-module = "management";
          inherit custom pkgs inputs self;
        };
        pihole = mkComputer {
          hostname = "pihole";
          inherit custom pkgs inputs self;
        };
        plex = mkComputer {
          hostname = "plex";
          inherit custom pkgs inputs self;
        };
        proxy = mkComputer {
          hostname = "proxy";
          inherit custom pkgs inputs self;
        };
        raspi-test = mkRaspi {
          hostname = "raspi-test";
          inherit custom pkgs inputs self;
        };
        restic-server = mkComputer {
          hostname = "restic-server";
          inherit custom pkgs inputs self;
        };
        test-server = mkComputer {
          hostname = "test-server";
          inherit custom pkgs inputs self;
        };
        ttrss = mkComputer {
          hostname = "ttrss";
          inherit custom pkgs inputs self;
        };
      };
      homeConfigurations = {
        "${custom.username}@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home-manager/work-wsl.nix;
          inherit system username;
          homeDirectory = "/home/${custom.username}";
          extraSpecialArgs = {
            inherit custom inputs;
          };
        };
      };
    };
}

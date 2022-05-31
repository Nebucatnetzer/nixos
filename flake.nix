{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

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
    , nixpkgs
    , nixpkgs-unstable
    , nixos-hardware
    , home-manager
    }:
    let
      custom = import ./custom;
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
        ];
      };

      mkComputer = { hostname, system ? "x86_64-linux", home-module ? "headless" }: nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = { inherit custom inputs; };
        modules = (
          [
            # System configuration for this host
            "${self}/systems/${hostname}"

            # Common configuration
            "${self}/modules/common-x86"

            agenix.nixosModules.age
            { environment.systemPackages = [ agenix.defaultPackage.${system} ]; }

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${custom.username}.imports = [
                (import "${self}/home-manager/${home-module}.nix" { inherit custom pkgs inputs; })
              ];
            }
          ]);
      };
      mkRaspi = { hostname, system ? "aarch64-linux", home-module ? "headless" }: nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit custom inputs; };
        modules = (
          [
            # System configuration for this host
            "${self}/systems/${hostname}"

            # Common configuration
            ./modules/common

            agenix.nixosModules.age
            { environment.systemPackages = [ agenix.defaultPackage.${system} ]; }

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${custom.username}.imports = [
                (import "${self}/home-manager/${home-module}.nix" { inherit custom pkgs inputs; })
              ];
            }
          ]);
      };
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer {
          hostname = "gwyn";
          home-module = "desktop";
        };
        nixos-vm = mkComputer {
          hostname = "desktop-vm";
          home-module = "desktop";
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
          home-module = "desktop";
        };
        # Servers
        git = mkComputer {
          hostname = "git";
        };
        jdownloader = mkComputer {
          hostname = "jdownloader";
        };
        mail = mkComputer {
          hostname = "mail";
        };
        nextcloud = mkComputer {
          hostname = "nextcloud";
        };
        nixos-management = mkComputer {
          hostname = "nixos-management";
          home-module = "management";
        };
        pihole = mkComputer {
          hostname = "pihole";
        };
        plex = mkComputer {
          hostname = "plex";
        };
        proxy = mkComputer {
          hostname = "proxy";
        };
        raspi-test = mkRaspi {
          hostname = "raspi-test";
        };
        restic-server = mkComputer {
          hostname = "restic-server";
        };
        test-server = mkComputer {
          hostname = "test-server";
        };
        ttrss = mkComputer {
          hostname = "ttrss";
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

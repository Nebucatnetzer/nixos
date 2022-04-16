{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
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

      mkComputer = { hostname, system ? "x86_64-linux" }: nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = { inherit custom inputs; };
        modules = (
          [
            # System configuration for this host
            "${self}/systems/${hostname}"

            # Common configuration
            (import ./modules/common-x86 { inherit custom inputs pkgs system; })

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${custom.username}.imports = [
                (import ./home-manager/desktop.nix { inherit custom pkgs inputs; })
              ];
            }
          ]);
      };
      mkVM = { hostname, system ? "x86_64-linux" }: nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          inherit custom inputs;
        };
        modules = (
          [
            # System configuration for this host
            "${self}/systems/${hostname}"

            # Common configuration
            ./modules/common-x86

            agenix.nixosModules.age
            { environment.systemPackages = [ agenix.defaultPackage.${system} ]; }

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${custom.username}.imports = [
                (import ./home-manager/management.nix { inherit custom pkgs inputs; })
              ];
            }
          ]);
      };
      mkRaspi = { hostname, system ? "aarch64-linux", home-module }: nixpkgs.lib.nixosSystem {
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
        };
        nixos-vm = mkComputer {
          hostname = "desktop-vm";
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
        };
        # Servers
        git = mkVM {
          hostname = "git";
        };
        grav = mkVM {
          hostname = "grav";
        };
        heimdall = mkVM {
          hostname = "heimdall";
        };
        jdownloader = mkVM {
          hostname = "jdownloader";
        };
        k3s-master1 = mkVM {
          hostname = "k3s-master1";
        };
        k3s-node1 = mkVM {
          hostname = "k3s-node1";
        };
        k3s-node2 = mkVM {
          hostname = "k3s-node2";
        };
        mail = mkVM {
          hostname = "mail";
        };
        nextcloud = mkVM {
          hostname = "nextcloud";
        };
        nixos-management = mkVM {
          hostname = "nixos-management";
        };
        nomad-master1 = mkVM {
          hostname = "nomad-master1";
        };
        nomad-client1 = mkVM {
          hostname = "nomad-client1";
        };
        pihole = mkVM {
          hostname = "pihole";
        };
        plex = mkVM {
          hostname = "plex";
        };
        proxy = mkVM {
          hostname = "proxy";
        };
        raspi-test = mkRaspi {
          hostname = "raspi-test";
          home-module = "management";
        };
        restic-server = mkVM {
          hostname = "restic-server";
        };
        rss-bridge = mkVM {
          hostname = "rss-bridge";
        };
        ttrss = mkVM {
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

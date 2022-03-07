{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self
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
      mkComputer = configurationNix: nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = { inherit custom inputs; };
        modules = (
          [
            # System configuration for this host
            configurationNix

            # Common configuration
            ./modules/common

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
      mkVM = configurationNix: nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = { inherit custom inputs; };
        modules = (
          [
            # System configuration for this host
            configurationNix

            # Common configuration
            ./modules/common

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${custom.username}.imports = [
                (import ./home-manager/headless.nix { inherit custom pkgs inputs; })
              ];
            }
          ]);
      };
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer ./systems/gwyn;
        nixos-vm = mkComputer ./systems/desktop-vm;
        staubfinger = mkComputer ./systems/staubfinger;
        # Servers
        git = mkVM ./systems/git;
        grav = mkVM ./systems/grav;
        heimdall = mkVM ./systems/heimdall;
        jdownloader = mkVM ./systems/jdownloader;
        k3s-master1 = mkVM ./systems/k3s-master1;
        k3s-node1 = mkVM ./systems/k3s-node1;
        k3s-node2 = mkVM ./systems/k3s-node2;
        mail = mkVM ./systems/mail;
        nextcloud = mkVM ./systems/nextcloud;
        nixos-management = mkVM ./systems/nixos-management;
        nomad-master1 = mkVM ./systems/nomad-master1;
        nomad-client1 = mkVM ./systems/nomad-client1;
        pihole = mkVM ./systems/pihole;
        plex = mkVM ./systems/plex;
        proxy = mkVM ./systems/proxy;
        restic-server = mkVM ./systems/restic-server;
        rss-bridge = mkVM ./systems/rss-bridge;
        ttrss = mkVM ./systems/ttrss;
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

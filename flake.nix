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
      mkComputer = configurationNix: homeManagerRole: nixpkgs.lib.nixosSystem {
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
                (import homeManagerRole { inherit custom pkgs inputs; })
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
                #(import homeManagerRole { inherit custom pkgs inputs; })
                ./home-manager/headless.nix;
              ];
            }
          ]);
      };
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer
          ./systems/gwyn
          ./home-manager/desktop.nix;
        staubfinger = mkComputer
          ./systems/staubfinger
          ./home-manager/desktop.nix;
        nixos-vm = mkComputer
          ./systems/desktop-vm
          ./home-manager/desktop.nix;
        # Servers
        proxy = mkComputer
          ./systems/proxy
          ./home-manager/headless.nix;
        nixos-management = mkComputer
          ./systems/nixos-management
          heimdall = mkComputer
        ./systems/heimdall
        ./home-manager/headless.nix;
        grav = mkComputer
          ./systems/grav
          ./home-manager/headless.nix;
        ttrss = mkComputer
          ./systems/ttrss
          ./home-manager/headless.nix;
        rss-bridge = mkComputer
          ./systems/rss-bridge
          ./home-manager/headless.nix;
        git = mkComputer
          ./systems/git
          ./home-manager/headless.nix;
        plex = mkComputer
          ./systems/plex
          ./home-manager/headless.nix;
        nextcloud = mkComputer
          ./systems/nextcloud
          ./home-manager/headless.nix;
        mail = mkComputer
          ./systems/mail
          ./home-manager/headless.nix;
        pihole = mkComputer
          ./systems/pihole
          ./home-manager/headless.nix;
        restic-server = mkComputer
          ./systems/restic-server
          ./home-manager/headless.nix;
        jdownloader = mkComputer
          ./systems/jdownloader
          ./home-manager/headless.nix;
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

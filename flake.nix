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
      mkComputer = configurationNix: homeManagerRole: extraModules: nixpkgs.lib.nixosSystem {
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
          ] ++ extraModules
        );
      };
      mkVM = import ./systems/proxmox-vm;
    in
    {
      nixosConfigurations = {
        gwyn = mkComputer
          ./systems/gwyn
          ./home-manager/desktop.nix
          [
            nixos-hardware.nixosModules.dell-precision-5530
            nixos-hardware.nixosModules.common-gpu-nvidia
            ./hardware/bluetooth
            ./hardware/nvidia
            ./modules/desktop
            ./modules/docker
            ./modules/droidcam
            ./modules/eog
            ./modules/espanso
            ./modules/lockscreen
            ./modules/nix-direnv
            ./modules/restic
          ];
        staubfinger = mkComputer
          ./systems/staubfinger
          ./home-manager/desktop.nix
          [
            nixos-hardware.nixosModules.common-pc-laptop
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            ./hardware/bluetooth
            ./modules/desktop
            ./modules/docker
            ./modules/droidcam
            ./modules/espanso
            ./modules/lockscreen
            ./modules/restic
          ];
        nixos-vm = mkComputer
          ./systems/desktop-vm
          ./home-manager/desktop.nix
          [
            ./modules/desktop
            ./modules/docker
            ./modules/espanso
          ];
        proxy = mkComputer
          ./systems/proxy
          ./home-manager/headless.nix
          [ ];
        nixos-management = mkComputer
          ./systems/nixos-management
          ./home-manager/headless.nix
          [ ];
        heimdall = mkComputer
          ./systems/heimdall
          ./home-manager/headless.nix
          [ ];
        grav = mkComputer
          ./systems/grav
          ./home-manager/headless.nix
          [ ];
        ttrss = mkComputer
          ./systems/ttrss
          ./home-manager/headless.nix
          [ ];
        rss-bridge = mkComputer
          ./systems/rss-bridge
          ./home-manager/headless.nix
          [ ];
        git = mkComputer
          ./systems/git
          ./home-manager/headless.nix
          [ ];
        plex = mkComputer
          ./systems/plex
          ./home-manager/headless.nix
          [ ];
        nextcloud = mkComputer
          ./systems/nextcloud
          ./home-manager/headless.nix
          [ ];
        mail = mkComputer
          ./systems/mail
          ./home-manager/headless.nix
          [ ];
        pihole = mkComputer
          ./systems/pihole
          ./home-manager/headless.nix
          [ ];
        restic-server = mkComputer
          ./systems/pihole
          ./home-manager/headless.nix
          [ ];

        jdownloader = mkComputer
          (mkVM
            { hostname = "jdownloader"; ip = "10.7.89.110"; inherit custom inputs; })
          ./home-manager/headless.nix
          [
            ./modules/docker
            ./modules/download-share
            ./modules/jdownloader
          ];
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

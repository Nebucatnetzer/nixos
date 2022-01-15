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
      system = "x86_64-linux";
      username = import ./username.nix;
      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          system = "x86_64-linux";
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
        specialArgs = { inherit system inputs; };
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
              home-manager.users.${username}.imports = [ homeManagerRole ];
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
            ./modules/lockscreen
            ./modules/restic
            ./modules/xonsh
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
            ./modules/lockscreen
            ./modules/restic
            ./modules/xonsh
          ];
        nixos-vm = mkComputer
          ./systems/desktop-vm
          ./home-manager/desktop.nix
          [
            ./modules/desktop
          ];
        nixos-test-vm = mkComputer
          (mkVM
            { hostname = "nixos-test-vm"; ip = "10.7.89.235"; })
          ./home-manager/headless.nix
          [
            ./modules/code-server
            ./modules/data-share
            ./modules/docker
          ];
        heimdall = mkComputer
          (mkVM
            { hostname = "heimdall"; ip = "10.7.89.121"; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        grav = mkComputer
          (mkVM
            { hostname = "grav"; ip = "10.7.89.102"; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];
      };
    };
}

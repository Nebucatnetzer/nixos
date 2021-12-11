{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs @ { self
    , nixpkgs
    , nixpkgs-unstable
    , nixos-hardware
    , home-manager
    }: {
      nixosConfigurations.gwyn = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hardware/precision/configuration.nix
          nixos-hardware.nixosModules.dell-precision-5530
          nixos-hardware.nixosModules.common-gpu-nvidia
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.andreas = import ./home-manager/desktop.nix;
          }
        ];
      };
    };
}

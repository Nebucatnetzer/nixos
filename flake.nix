{
  description = "Andreas Zweili's Nixos configuration";
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darktable-initial-workflow = {
      url = "github:UliGesing/Darktable-Initial-Workflow-Module";
      flake = false;
    };
    darktable-lua-scripts = {
      url = "github:darktable-org/lua-scripts";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fox-flss = {
      url = "github:Nebucatnetzer/fii_linux?ref=flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs =
    inputs@{
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      ...
    }:
    let
      mkComputer = import ./lib/mk_computer.nix;
      hosts = {
        "capricorn" = {
          home-module = "desktop";
        };
        "gwyn" = {
          home-module = "management";
        };
      };
      hostConfigs = nixpkgs.lib.attrsets.mapAttrs (
        hostname:
        {
          home-module ? "headless",
        }:
        (mkComputer {
          inherit
            inputs
            hostname
            home-module
            unstable-pkgs
            ;
        })
      ) hosts;
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
      unstable-pkgs = import nixpkgs-unstable {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
    in
    {
      nixosConfigurations = hostConfigs;
      devShells."x86_64-linux".default = pkgs.callPackage ./shell.nix { };
      packages."x86_64-linux" = {
        inherit pkgs;
        azPkgs = import ./pkgs { inherit pkgs unstable-pkgs; };
      };
      homeConfigurations = {
        "zweili@CO-NB-102" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./modules/home-manager/profiles/work-wsl.nix ];
          extraSpecialArgs = {
            inherit inputs unstable-pkgs;
            nixosConfig = {
              az-hosts = import "${inputs.self}/modules/misc/hosts/hosts.nix";
              az-username = "zweili";
            };
          };
        };
      };
    };
}

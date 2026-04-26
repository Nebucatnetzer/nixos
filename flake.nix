{
  description = "Andreas Zweili's Nixos configuration";
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fox-flss.url = "github:Nebucatnetzer/fii_linux?ref=flake";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs =
    inputs@{
      home-manager,
      nixpkgs,
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
        (mkComputer { inherit inputs hostname home-module; })
      ) hosts;
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      nixosConfigurations = hostConfigs;
      devShells."x86_64-linux".default = pkgs.callPackage ./shell.nix { };
      packages."x86_64-linux" = {
        inherit pkgs;
        azPkgs = import ./pkgs { inherit inputs pkgs; };
      };
      homeConfigurations = {
        "zweili@CO-NB-102" = home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
          modules = [ ./modules/home-manager/profiles/work-wsl.nix ];
          extraSpecialArgs = {
            inherit inputs;
            nixosConfig = {
              az-hosts = import "${inputs.self}/modules/misc/hosts/hosts.nix";
              az-username = "zweili";
            };
          };
        };
      };
    };
}

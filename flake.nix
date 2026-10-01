{
  description = "Andreas Zweili's Nixos configuration";
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fox-flss = {
      url = "github:Nebucatnetzer/fii_linux?ref=flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.zst";
    nixpkgs-unstable.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.zst";
    # Zotero is broken in unstable
    nixpkgs-zotero.url = "github:NixOS/nixpkgs/363fdbe57ed052c76e816e6270206b0cb348e53a";
    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs =
    inputs@{
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-zotero,
      ...
    }:
    let
      mkComputer = import ./lib/mk_computer.nix;
      hosts = {
        "capricorn" = {
          home-module = "desktop";
        };
        "fenoglio" = {
          home-module = "management";
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
        overlays = [
          # Zotero comes from the pinned commit, with the dependencies it was built
          # against there.
          (final: prev: { inherit (zotero-pkgs) zotero; })
        ];
      };
      zotero-pkgs = import nixpkgs-zotero {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
    in
    {
      nixosConfigurations = hostConfigs;
      devShells."x86_64-linux".default = pkgs.callPackage ./shell.nix { inherit pkgs unstable-pkgs; };
      packages."x86_64-linux" = {
        inherit pkgs;
        azPkgs = import ./pkgs { inherit pkgs unstable-pkgs; };
        az-emacs = (import ./pkgs { inherit pkgs unstable-pkgs; }).az-emacs;
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

{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      agenix,
      nixpkgs,
      nixpkgs-unstable,
      nixos-hardware,
      home-manager,
    }:
    let
      mkComputer = import "${inputs.self}/lib/mk_computer.nix";
      mkRaspi = import "${inputs.self}/lib/mk_raspi.nix";
      mksdImage =
        host:
        (self.nixosConfigurations.${host}.extendModules {
          modules = [ "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix" ];
        }).config.system.build.sdImage;
      # required for home-manager only setup {
      overlay-unstable = final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
      };
      pkgs = import inputs.nixpkgs {
        system = "x86_64-linux";
        config = {
          allowUnfree = true;
        };
        overlays = [ overlay-unstable ];
      };
      # }
      raspis = {
        "git" = { };
        "plex" = { };
        "proxy" = { };
        "mail" = { };
        "management" = {
          home-module = "management";
        };
        "nextcloud" = { };
        "restic-server" = { };
        "ttrss" = { };
      };
      raspiConfigs = nixpkgs.lib.attrsets.mapAttrs (
        hostname:
        {
          home-module ? "headless",
        }:
        (mkRaspi { inherit inputs hostname home-module; })
      ) raspis;

      pcs = {
        "gwyn" = {
          home-module = "desktop";
        };
        "staubfinger" = {
          home-module = "desktop";
        };
        "desktop-vm" = {
          home-module = "work-vm";
        };
      };
      pcConfigs = nixpkgs.lib.attrsets.mapAttrs (
        hostname:
        {
          home-module ? "headless",
        }:
        (mkComputer { inherit inputs hostname home-module; })
      ) pcs;
    in
    {
      images = {
        git = mksdImage "git";
        plex = mksdImage "plex";
        proxy = mksdImage "proxy";
        mail = mksdImage "mail";
        management = mksdImage "management";
        nextcloud = mksdImage "nextcloud";
        restic-server = mksdImage "restic-server";
        ttrss = mksdImage "ttrss";
      };
      nixosConfigurations = raspiConfigs // pcConfigs;
      homeConfigurations = {
        "zweili@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            "${inputs.self}/home-manager/profiles/work-wsl.nix"
            inputs.agenix.homeManagerModules.age
          ];
          extraSpecialArgs = {
            inherit inputs;
            nixosConfig = {
              az-username = "zweili";
            };
          };
        };
      };
    };
}

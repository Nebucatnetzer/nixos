{
  description = "Andreas Zweili's Nixos configuration";
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs =
    inputs@{
      home-manager,
      nixpkgs,
      self,
      ...
    }:
    let
      mkComputer = import ./lib/mk_computer.nix;
      mksdImage =
        host:
        (self.nixosConfigurations.${host}.extendModules {
          modules = [ "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix" ];
        }).config.system.build.sdImage;
      hosts = {
        "budget" = { };
        "desktop-vm" = {
          home-module = "work-vm";
        };
        "git" = { };
        "gwyn" = {
          home-module = "desktop";
        };
        "plex" = { };
        "proxy" = { };
        "mail" = { };
        "management" = {
          home-module = "management";
        };
        "nextcloud" = { };
        "restic-server" = { };
        "staubfinger" = {
          home-module = "desktop";
        };
        "ttrss" = { };
      };
      hostConfigs = nixpkgs.lib.attrsets.mapAttrs (
        hostname:
        {
          home-module ? "headless",
        }:
        (mkComputer { inherit inputs hostname home-module; })
      ) hosts;
    in
    {
      images = nixpkgs.lib.attrsets.mapAttrs (name: _: mksdImage name) hosts;
      nixosConfigurations = hostConfigs;
      homeConfigurations = {
        "zweili@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
          modules = [ ./home-manager/profiles/work-wsl.nix ];
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

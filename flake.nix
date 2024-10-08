{
  description = "Andreas Zweili's Nixos configuration";
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kmonad = {
      url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
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
      mkRaspi = import ./lib/mk_raspi.nix;
      mksdImage =
        host:
        (self.nixosConfigurations.${host}.extendModules {
          modules = [ "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix" ];
        }).config.system.build.sdImage;
      raspis = {
        "budget" = { };
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
      images = nixpkgs.lib.attrsets.mapAttrs (name: _: mksdImage name) raspis;
      nixosConfigurations = raspiConfigs // pcConfigs;
      homeConfigurations =
        let
          system = "x86_64-linux";
          pkgs = import inputs.nixpkgs {
            config = {
              allowUnfree = true;
            };
            inherit system;
          };
        in
        {
          "zweili@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./home-manager/profiles/work-wsl.nix ];
            extraSpecialArgs = {
              inherit inputs system;
              nixosConfig = {
                az-username = "zweili";
              };
            };
          };
        };
    };
}

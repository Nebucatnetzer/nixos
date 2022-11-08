{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self
    , agenix
    , nixpkgs
    , nixpkgs-unstable
    , nixos-hardware
    , home-manager
    }:
    let
      custom = import ./custom { inherit inputs; };
      mkComputer = import "${custom.inputs.self}/lib/mk_computer.nix";
      mkRaspi = import "${custom.inputs.self}/lib/mk_raspi.nix";
      mksdImage = host: (self.nixosConfigurations.${host}.extendModules {
        modules = [ "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix" ];
      }).config.system.build.sdImage;
    in
    {
      images = {
        git = mksdImage "git";
        loki-test = mksdImage "loki-test";
        plex = mksdImage "plex";
        proxy = mksdImage "proxy";
        mail = mksdImage "mail";
        management = mksdImage "management";
        nextcloud = mksdImage "nextcloud";
        pihole = mksdImage "pihole";
      };
      nixosConfigurations = {
        gwyn = mkComputer {
          hostname = "gwyn";
          home-module = "desktop";
          inherit custom;
        };
        desktop-vm = mkComputer {
          hostname = "desktop-vm";
          home-module = "desktop";
          inherit custom;
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
          home-module = "desktop";
          inherit custom;
        };
        # Servers
        git = mkRaspi {
          hostname = "git";
          inherit custom;
        };
        loki-test = mkRaspi {
          hostname = "loki-test";
          inherit custom;
        };
        mail = mkRaspi {
          hostname = "mail";
          inherit custom;
        };
        nextcloud = mkRaspi {
          hostname = "nextcloud";
          inherit custom;
        };
        pihole = mkRaspi {
          hostname = "pihole";
          inherit custom;
        };
        plex = mkRaspi {
          hostname = "plex";
          home-module = "plex";
          inherit custom;
        };
        proxy = mkRaspi {
          hostname = "proxy";
          inherit custom;
        };
        management = mkRaspi {
          hostname = "management";
          home-module = "management";
          inherit custom;
        };
        restic-server = mkComputer {
          hostname = "restic-server";
          inherit custom;
        };
        ttrss = mkComputer {
          hostname = "ttrss";
          inherit custom;
        };
      };
      homeConfigurations = {
        "${custom.username}@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          configuration = import "${custom.inputs.self}/home-manager/work-wsl.nix";
          system = "x86_64-linux";
          username = custom.username;
          homeDirectory = "/home/${custom.username}";
          extraSpecialArgs = {
            inherit custom;
          };
        };
      };
    };
}

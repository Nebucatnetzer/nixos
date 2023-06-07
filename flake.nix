{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # look here for the hardware options https://github.com/NixOS/nixos-hardware/blob/master/flake.nix#L5
    nixos-hardware.url = "github:nixos/nixos-hardware?rev=b3a8d308a13390df35b198d4db36a654ec29e25a";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
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
      mkComputer = import "${inputs.self}/lib/mk_computer.nix";
      mkRaspi = import "${inputs.self}/lib/mk_raspi.nix";
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
        mobile = mksdImage "mobile";
        nextcloud = mksdImage "nextcloud";
        test-raspi = mksdImage "test-raspi";
        restic-server = mksdImage "restic-server";
        ttrss = mksdImage "ttrss";
      };
      nixosConfigurations = {
        gwyn = mkComputer {
          hostname = "gwyn";
          home-module = "desktop";
          inherit inputs;
        };
        desktop-vm = mkComputer {
          hostname = "desktop-vm";
          home-module = "desktop";
          inherit inputs;
        };
        staubfinger = mkComputer {
          hostname = "staubfinger";
          home-module = "desktop";
          inherit inputs;
        };
        # Servers
        git = mkRaspi {
          hostname = "git";
          inherit inputs;
        };
        loki-test = mkRaspi {
          hostname = "loki-test";
          inherit inputs;
        };
        mail = mkRaspi {
          hostname = "mail";
          inherit inputs;
        };
        nextcloud = mkRaspi {
          hostname = "nextcloud";
          inherit inputs;
        };
        plex = mkRaspi {
          hostname = "plex";
          inherit inputs;
        };
        proxy = mkRaspi {
          hostname = "proxy";
          inherit inputs;
        };
        management = mkRaspi {
          hostname = "management";
          home-module = "management";
          inherit inputs;
        };
        mobile = mkRaspi {
          hostname = "mobile";
          home-module = "management";
          inherit inputs;
        };
        restic-server = mkRaspi {
          hostname = "restic-server";
          inherit inputs;
        };
        test-raspi = mkRaspi {
          hostname = "test-raspi";
          inherit inputs;
        };
        ttrss = mkRaspi {
          hostname = "ttrss";
          inherit inputs;
        };
      };
      homeConfigurations = {
        "zweili@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            config = {
              allowUnfree = true;
            };
          };
          modules = [
            "${inputs.self}/home-manager/work-wsl.nix"
          ];
          extraSpecialArgs = {
            inherit inputs;
          };
        };
      };
    };
}

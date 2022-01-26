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
      username = "andreas";
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
        specialArgs = { inherit self system inputs username; };
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
              home-manager.users.${username}.imports = [
                (import homeManagerRole { inherit pkgs username; })
              ];
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
            ./modules/eog
            ./modules/espanso
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
            ./modules/espanso
            ./modules/lockscreen
            ./modules/restic
            ./modules/xonsh
          ];
        nixos-vm = mkComputer
          ./systems/desktop-vm
          ./home-manager/desktop.nix
          [
            ./modules/desktop
            ./modules/espanso
            ./modules/xonsh
          ];
        proxy = mkComputer
          (mkVM
            { hostname = "proxy"; ip = "10.7.89.100"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/haproxy
          ];
        nixos-management = mkComputer
          (mkVM
            { hostname = "nixos-management"; ip = "10.7.89.150"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/code-server
            ./modules/docker
            ./modules/xonsh
          ];
        heimdall = mkComputer
          (mkVM
            { hostname = "heimdall"; ip = "10.7.89.121"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        grav = mkComputer
          (mkVM
            { hostname = "grav"; ip = "10.7.89.102"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        ttrss = mkComputer
          (mkVM
            { hostname = "ttrss"; ip = "10.7.89.115"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        rss-bridge = mkComputer
          (mkVM
            { hostname = "rss-bridge"; ip = "10.7.89.111"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        git = mkComputer
          (mkVM
            { hostname = "git"; ip = "10.7.89.109"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        plex = mkComputer
          (mkVM
            { hostname = "plex"; ip = "10.7.89.112"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
            ./modules/media-share
            ./modules/plex
          ];

        nextcloud = mkComputer
          (mkVM
            { hostname = "nextcloud"; ip = "10.7.89.103"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
            ./modules/media-share
          ];

        mail = mkComputer
          (mkVM
            { hostname = "mail"; ip = "10.7.89.123"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
          ];

        pihole = mkComputer
          (mkVM
            { hostname = "pihole"; ip = "10.7.89.2"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/docker
            ./modules/pihole
            ./modules/unbound
          ];

        backup = mkComputer
          (mkVM
            { hostname = "backup"; ip = "10.7.89.117"; inherit self; })
          ./home-manager/headless.nix
          [
            ./modules/restic-share
          ];
      };
      homeConfigurations = {
        "${username}@co-ws-con4" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home-manager/work-wsl.nix;
          inherit system username;
          homeDirectory = "/home/${username}";
          extraSpecialArgs = {
            inherit self system username;
          };
        };
      };
    };
}

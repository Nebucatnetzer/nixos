{ custom, hostname, inputs, system ? "x86_64-linux", home-module ? "headless", username ? "andreas" }:
let
  overlay-unstable = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
  };

  pkgs = import inputs.nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
    };
    overlays = [
      overlay-unstable
      #      (final: prev: {
      #        nextcloud-client = prev.nextcloud-client.overrideAttrs (_: rec {
      #          version = "3.6.0";
      #          src = pkgs.fetchFromGitHub {
      #            owner = "nextcloud";
      #            repo = "desktop";
      #            rev = "v${version}";
      #            sha256 = "sha256-wAxq5xFlofn2xEguvewMvGcel9O+CN/1AycR3tv/xMA=";
      #          };
      #        });
      #      })
    ];
  };
in
inputs.nixpkgs.lib.nixosSystem
{
  inherit system pkgs;
  specialArgs = { inherit inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${inputs.self}/systems/${hostname}" {
        inherit hostname;
      })

      # Common configuration
      "${inputs.self}/modules"

      inputs.agenix.nixosModules.age
      {
        environment.systemPackages = [ inputs.agenix.packages.${system}.default ];
        az-username = username;
      }

      inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.andreas.imports = [
          (import "${inputs.self}/home-manager/${home-module}.nix" { inherit custom system; })
        ];
      }
    ]);
}

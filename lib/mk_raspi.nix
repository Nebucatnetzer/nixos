{ custom, hostname, system ? "aarch64-linux", home-module ? "headless" }:
let
  overlay-unstable = final: prev: {
    unstable = import custom.inputs.nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
  };

  pkgs = import custom.inputs.nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
    };
    overlays = [
      overlay-unstable
      custom.inputs.nix-alien.overlay
    ];
  };
in

custom.inputs.nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = { inherit custom; };
  modules = (
    [
      # System configuration for this host
      (import "${custom.inputs.self}/systems/${hostname}"
        { inherit custom hostname; })

      # Common configuration
      (import "${custom.inputs.self}/modules/common" { inherit custom; })

      custom.inputs.agenix.nixosModules.age
      { environment.systemPackages = [ custom.inputs.agenix.defaultPackage.${system} ]; }

      custom.inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${custom.username}.imports = [
          (import "${custom.inputs.self}/home-manager/${home-module}.nix" { inherit custom; })
        ];
      }
    ]);
}

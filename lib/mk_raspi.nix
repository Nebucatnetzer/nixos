{ custom, hostname, inputs, system ? "aarch64-linux", home-module ? "headless" }:
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
      inputs.nix-alien.overlay
    ];
  };
in
inputs.nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = { inherit custom inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${inputs.self}/systems/${hostname}"
        { inherit custom hostname inputs; })

      # Common configuration
      (import "${inputs.self}/modules/common" { inherit custom inputs; })

      inputs.agenix.nixosModules.age
      { environment.systemPackages = [ inputs.agenix.defaultPackage.${system} ]; }

      inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${custom.username}.imports = [
          (import "${inputs.self}/home-manager/${home-module}.nix" { inherit custom inputs; })
        ];
      }
    ]);
}

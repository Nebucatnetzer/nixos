{ hostname, inputs, system ? "aarch64-linux", home-module ? "headless", username ? "andreas" }:
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
      # The following is requried for building images {
      # https://github.com/NixOS/nixpkgs/issues/126755#issuecomment-869149243
      (final: super: {
        makeModulesClosure = x:
          super.makeModulesClosure (x // { allowMissing = true; });
      })
      # }
    ];
  };
in

inputs.nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = { inherit inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${inputs.self}/systems/${hostname}"
        { inherit hostname; })

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
        home-manager.extraSpecialArgs = { inherit inputs system; };
        home-manager.users.${username}.imports = [
          "${inputs.self}/home-manager/${home-module}.nix"
        ];
      }
    ]);
}

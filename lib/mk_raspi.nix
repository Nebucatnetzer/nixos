{ custom, hostname, inputs, system ? "aarch64-linux", home-module ? "headless", username ? "andreas" }:
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

custom.inputs.nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = { inherit custom inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${custom.inputs.self}/systems/${hostname}"
        { inherit hostname; })

      # Common configuration
      "${custom.inputs.self}/modules"

      custom.inputs.agenix.nixosModules.age
      {
        environment.systemPackages = [ custom.inputs.agenix.packages.${system}.default ];
        az-username = username;
      }

      custom.inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${username}.imports = [
          (import "${custom.inputs.self}/home-manager/${home-module}.nix" { inherit custom system; })
        ];
      }
    ]);
}

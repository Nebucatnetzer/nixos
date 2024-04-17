{
  home-module,
  hostname,
  inputs,
  username ? "andreas",
}:
let
  system = "aarch64-linux";
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
      (final: prev: {
        freshrss = prev.freshrss.overrideAttrs (_: {
          version = "1.23.1";
          src = pkgs.fetchFromGitHub {
            owner = "FreshRSS";
            repo = "FreshRSS";
            rev = "c89073d60e491f775a13a9ec57915313eb073964";
            sha256 = "sha256-DqfkbfvqGkAMQ2oawfb7Ggiv2u6/Qq6UgygLTNov9CA=";
          };
        });
      })

      # The following is requried for building images {
      # https://github.com/NixOS/nixpkgs/issues/126755#issuecomment-869149243
      (final: super: {
        makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
      })
      # }
    ];
  };
in
inputs.nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = {
    inherit inputs;
  };
  modules = ([
    # System configuration for this host
    (import "${inputs.self}/systems/${hostname}" { inherit hostname; })

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
      home-manager.extraSpecialArgs = {
        inherit inputs system;
      };
      home-manager.users.${username}.imports = [
        inputs.agenix.homeManagerModules.default
        "${inputs.self}/home-manager/profiles/${home-module}.nix"
      ];
    }
  ]);
}

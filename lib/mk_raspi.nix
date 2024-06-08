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
        freshrss = prev.freshrss.overrideAttrs (
          _:
          let
            version = "1.24.1";
          in
          {
            inherit version;
            src = pkgs.fetchFromGitHub {
              owner = "FreshRSS";
              repo = "FreshRSS";
              rev = version;
              sha256 = "sha256-AAOON1RdbG6JSnCc123jmIlIXHOE1PE49BV4hcASO/s=";
            };
          }
        );
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
    {
      az-username = username;
      home-manager.users.${username}.imports = [
        "${inputs.self}/home-manager/profiles/${home-module}.nix"
      ];
    }
  ]);
}

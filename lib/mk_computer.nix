{ custom, hostname, inputs, pkgs, system ? "x86_64-linux", home-module ? "headless" }: inputs.nixpkgs.lib.nixosSystem {
  inherit system pkgs;
  specialArgs = { inherit custom inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${inputs.self}/systems/${hostname}" {
        lib = pkgs.lib;
        inherit custom inputs hostname pkgs;
      })

      # Common configuration
      "${inputs.self}/modules/common-x86"

      inputs.agenix.nixosModules.age
      { environment.systemPackages = [ inputs.agenix.defaultPackage.${system} ]; }

      inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${custom.username}.imports = [
          (import "${inputs.self}/home-manager/${home-module}.nix" { inherit custom pkgs inputs; })
        ];
      }
    ]);
}

{ custom, hostname, inputs, pkgs, system ? "aarch64-linux", home-module ? "headless" }: inputs.nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = { inherit custom inputs; };
  modules = (
    [
      # System configuration for this host
      (import "${inputs.self}/systems/${hostname}"
        { inherit custom hostname inputs pkgs; })

      # Common configuration
      "${inputs.self}/modules/common"

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

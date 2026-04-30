{
  home-module,
  hostname,
  inputs,
  unstable-pkgs,
  username ? "andreas",
}:
inputs.nixpkgs.lib.nixosSystem {
  specialArgs = {
    inherit inputs unstable-pkgs;
  };
  modules = [
    # System configuration for this host
    (import "${inputs.self}/systems/${hostname}" { inherit hostname; })
    # Common configuration
    "${inputs.self}/modules"
    {
      az-hosts = import "${inputs.self}/modules/misc/hosts/hosts.nix";
      az-username = username;
      home-manager.backupFileExtension = "hmbpk";
      home-manager.extraSpecialArgs = {
        inherit inputs unstable-pkgs;
      };
      home-manager.users.${username}.imports = [
        "${inputs.self}/modules/home-manager/profiles/${home-module}.nix"
      ];

    }
  ];
}

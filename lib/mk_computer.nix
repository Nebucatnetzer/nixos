{
  home-module,
  hostname,
  inputs,
  username ? "andreas",
}:
inputs.nixpkgs.lib.nixosSystem {
  specialArgs = {
    inherit inputs;
  };
  modules = [
    # System configuration for this host
    (import "${inputs.self}/systems/${hostname}" { inherit hostname; })
    # Common configuration
    "${inputs.self}/modules"
    {
      az-username = username;
      home-manager.backupFileExtension = "hmbpk";
      home-manager.users.${username}.imports = [
        "${inputs.self}/home-manager/profiles/${home-module}.nix"
      ];

    }
  ];
}

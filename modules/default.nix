{ inputs, ... }:
{
  imports = [
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager
    ./misc/common
    ./misc/hosts
    ./misc/storage-box/options.nix
    ./misc/username
  ];
}

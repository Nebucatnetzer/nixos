{ inputs, ... }:
{
  imports = [
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager
    ./misc/common
    ./misc/username
    ./services/az-restic-server
  ];
}

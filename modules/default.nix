{ inputs, ... }:
{
  imports = [
    inputs.agenix.nixosModules.age
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    ./misc/common
    ./misc/username
  ];
}

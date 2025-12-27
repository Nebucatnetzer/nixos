{ inputs, ... }:
{
  imports = [
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager
    ./misc/common
    ./misc/username
    ./services/az-librenms-agent
    ./services/az-nginx-fpm
    ./services/az-nginx-proxy
    ./services/az-restic-client-server
    ./services/az-restic-client-server-mysql
    ./services/az-restic-client-server-postgres
    ./services/az-restic-server
  ];
}

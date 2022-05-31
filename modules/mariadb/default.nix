{ pkgs, ... }:
{
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    settings = {
      mysqld = {
        bind-address = "172.17.0.1";
      };
    };
  };
  networking.firewall.interfaces."docker0".allowedTCPPorts = [ 3306 ];
}

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
  networking.firewall.extraCommands = "iptables -A INPUT -p tcp --destination-port 3306 -s 172.16.0.0/12 -j ACCEPT";
}

{ pkgs, ... }:
{
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    bind = "172.18.0.1";
  };
}

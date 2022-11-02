{ pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    settings.listen_adresses = pkgs.lib.mkForce "127.0.0.1,172.17.0.1";
  };
  networking.firewall.extraCommands = "iptables -A INPUT -p tcp --destination-port 3306 -s 172.16.0.0/12 -j ACCEPT";
}

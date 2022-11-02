{ ... }:
{
  services.postgresql = {
    enable = true;
    enableTCPIP = true;
  };
  networking.firewall.extraCommands = "iptables -A INPUT -p tcp --destination-port 3306 -s 172.16.0.0/12 -j ACCEPT";
}

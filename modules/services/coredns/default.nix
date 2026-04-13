{ ... }:
{
  services.coredns = {
    enable = true;
    config = builtins.readFile ./coredns.conf;
  };
  networking.firewall.allowedUDPPorts = [ 53 ];
}

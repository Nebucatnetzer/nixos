{ ... }:
{
  services.nomad = {
    enable = true;
    settings = {
      server = {
        enabled = true;
        bootstrap_expect = 1; # for demo; no fault tolerance
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 4646 ];
}

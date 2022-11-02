{ config, inputs, ... }:
{
  networking = {
    firewall.allowedTCPPorts = [
      53 # DNS
      67 # DHCP
      80 # Web Interface
    ];
    firewall.allowedUDPPorts = [
      53 # DNS
      67 # DHCP
    ];
  };
  age.secrets.piholeEnv.file = "${inputs.self}/scrts/pihole_env.age";
  virtualisation.oci-containers = {
    containers."pihole" = {
      image = "pihole/pihole";
      autoStart = true;
      environment = {
        TZ = "Europe/Zurich";
        ServerIP = "10.7.89.2";
        DNS1 = "127.0.0.1#5335"; # we're using the local unboud server here
        RATE_LIMIT = "10000/60";
      };
      environmentFiles = [ config.age.secrets.piholeEnv.path ];
      volumes = [
        "/var/lib/pihole/etc-pihole:/etc/pihole/"
        "/var/lib/pihole/etc-dnsmasq.d:/etc/dnsmasq.d/"
      ];
      extraOptions = [
        "--network=host"
        "--cap-add=NET_ADMIN"
      ];
    };
  };
}

{ custom }: { config, ... }:
let
  service-name = "${config.virtualisation.oci-containers.backend}-pihole";
in
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
  age.secrets.piholeEnv.file = "${custom.inputs.self}/scrts/pihole_env.age";
  virtualisation.oci-containers = {
    backend = "docker";
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
        "/etc/timezone:/etc/timezone:ro"
        "/etc/localtime:/etc/localtime:ro"
      ];
      extraOptions = [
        "--network=host"
        "--cap-add=NET_ADMIN"
      ];
    };
  };
  systemd.services.${service-name}.after = [ "unbound.service" ];
}

{ ... }:
{
  services.ntp = {
    enable = true;
    servers = [
      "loki.2li.local"
    ];
  };
}

{ ... }:
{
  services.ntp = {
    enable = true;
    server = [
      "loki.2li.local"
    ];
  };
}

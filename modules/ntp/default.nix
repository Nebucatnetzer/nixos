{ ... }:
{
  services.ntp = {
    enable = true;
    servers = [
      "10.7.89.1"
    ];
  };
}

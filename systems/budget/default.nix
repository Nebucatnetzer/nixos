{ hostname }:
{
  config,
  ...
}:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.113";
    };
  };

  profiles.az-server.enable = true;
  services = {
    az-actualbudget.enable = true;
    az-eactualbudget.enable = true;
  };
}

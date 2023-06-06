{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.30";
      inherit hostname;
    })
  ];
  services = {
    az-restic-server.enable = true;
  };
}

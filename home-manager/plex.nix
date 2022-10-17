{ inputs, custom, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/home-manager/headless" { inherit custom inputs pkgs; })
    (import "${inputs.self}/home-manager/podget" {
      downloadDir = "/mnt/media/podcasts";
      inherit custom inputs pkgs;
    })
  ];
}

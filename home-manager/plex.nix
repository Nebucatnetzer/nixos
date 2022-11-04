{ custom, inputs }: { ... }:
{
  imports = [
    (import "${inputs.self}/home-manager/headless.nix" { inherit custom inputs; })
    (import "${inputs.self}/home-manager/software/podget" {
      downloadDir = "/mnt/media/podcasts";
    })
  ];
}

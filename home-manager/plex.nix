{ custom }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/home-manager/headless.nix" { inherit custom; })
    (import "${custom.inputs.self}/home-manager/software/podget" {
      downloadDir = "/mnt/media/podcasts";
    })
  ];
}

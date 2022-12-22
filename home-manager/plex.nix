{ custom }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/home-manager/headless.nix" { inherit custom; })
  ];
}

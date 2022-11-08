{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.30";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-server" { inherit custom; })
  ];
}

{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/tmux"
  ];
}

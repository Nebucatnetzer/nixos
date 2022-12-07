{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/pihole" { inherit custom; }) # needs to be limited to lan interface
    "${custom.inputs.self}/modules/tmux"
    "${custom.inputs.self}/modules/unbound" # needs to be limited to lan interface
  ];
}

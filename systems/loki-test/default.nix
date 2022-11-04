{ custom, hostname, inputs }: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.10";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    (import "${inputs.self}/modules/pihole" { inherit inputs; }) # needs to be limited to lan interface
    "${inputs.self}/modules/router"
    "${inputs.self}/modules/tmux"
    "${inputs.self}/modules/unbound" # needs to be limited to lan interface
  ];
}

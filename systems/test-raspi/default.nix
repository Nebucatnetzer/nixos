{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.40";
      inherit hostname custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/nix-direnv" { inherit custom; })
    "${custom.inputs.self}/modules/tmux"
  ];
}

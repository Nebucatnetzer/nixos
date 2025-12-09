{ inputs, ... }:
{
  imports = [ "${inputs.self}/home-manager" ];

  targets.genericLinux.enable = true;
}

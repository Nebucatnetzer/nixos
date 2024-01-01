{ inputs, ... }: {
  imports = [ "${inputs.self}/home-manager/modules" ];

  targets.genericLinux.enable = true;
}

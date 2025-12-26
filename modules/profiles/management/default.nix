{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [
    "${inputs.self}/modules/programs/nix-direnv"
    "${inputs.self}/modules/programs/restic-management"
    "${inputs.self}/modules/programs/scripts"
  ];

  documentation = {
    man.generateCaches = false;
    nixos.includeAllModules = true;
  };

  age.identityPaths = [ "/home/${config.az-username}/.ssh/id_rsa" ];

  age.secrets.infomaniakEnv = {
    file = "${inputs.self}/scrts/infomaniak_env.age";
    mode = "600";
    owner = config.az-username;
    group = "users";
  };
  age.secrets.resticKey = {
    file = "${inputs.self}/scrts/restic.key.age";
    mode = "600";
    owner = config.az-username;
    group = "users";
  };

  # taken from here: https://github.com/NixOS/nixpkgs/blob/nixos-22.11/nixos/modules/hardware/video/hidpi.nix
  # {
  # Needed when typing in passwords for full disk encryption
  console.earlySetup = true;
  boot.loader.systemd-boot.consoleMode = "1";
  # }

  environment.systemPackages = [
    # what I consider to be system packages
    pkgs.lm_sensors
    pkgs.nixos-rebuild-ng
    pkgs.p7zip
    pkgs.podman-compose
    pkgs.quickemu
    pkgs.unrar
  ];
  virtualisation.podman.enable = true;
}

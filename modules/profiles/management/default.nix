{
  config,
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs unstable-pkgs; };
in
{
  imports = [
    "${inputs.self}/modules/programs/nix-direnv"
    "${inputs.self}/modules/programs/restic-management"
    "${inputs.self}/modules/programs/scripts"
    # Alongside restic-management deliberately: a host with the restore helpers but no
    # Storage Box access would have a half-useful profile, so they arrive together.
    (import "${inputs.self}/modules/misc/storage-box" {
      host = "u662087.your-storagebox.de";
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
      path = "backups/restic";
      user = "u662087";
    })
  ];

  documentation = {
    man.cache.enable = false;
    nixos.includeAllModules = true;
  };

  age.identityPaths = [ "/home/${config.az-username}/.ssh/id_rsa" ];

  age.secrets.resticKey = {
    file = "${inputs.self}/scrts/restic.key.age";
    mode = "440";
    owner = config.az-username;
    group = if config.users.users ? "restic" then "restic" else config.az-username;
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

    # moved from the management home-manager profile
    azPkgs.date-to-filename
    azPkgs.denote-rename
    azPkgs.rebuild
    azPkgs.unlock-luks
    azPkgs.update-file-dates
    pkgs.exercism
    pkgs.gh # GitHub CLI for working on poetry2nix
    pkgs.git
    pkgs.nix-prefetch-github
    pkgs.nix-prefetch-scripts
    pkgs.nix-tree
    pkgs.nps
    pkgs.termscp
    pkgs.trippy # network diagnostics
  ];
  programs.ssh.startAgent = true;
  virtualisation.podman.enable = true;
}

{
  inputs,
  pkgs,
  ...
}:
let
  date-to-filename = pkgs.callPackage "${inputs.self}/pkgs/date-to-filename" { };
  denote-rename = pkgs.callPackage "${inputs.self}/pkgs/denote-rename" { };
  update-file-dates = pkgs.callPackage "${inputs.self}/pkgs/update-file-dates" { };
  git = import "${inputs.self}/modules/home-manager/programs/git" { };
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    runtimeInputs = [
      pkgs.nixos-rebuild-ng
    ];
    text = ''
      if ${pkgs.netcat}/bin/nc -vzw 2 cache.zweili.org 2222; then
        nixos-rebuild-ng -j auto switch --sudo
        upload-to-cache /run/current-system
      else
        echo "Build without private cache"
        sudo nixos-rebuild-ng switch --option substituters "https://cache.nixos.org https://cache.nixos.org https://devenv.cachix.org"
      fi
    '';
  };
  unlock-luks = pkgs.writeShellScriptBin "unlock-luks" ''
    until ${pkgs.netcat}/bin/nc -vzw 2 $1 22; do
        sleep 1
    done &&
        ${pkgs.openssh}/bin/ssh \
          -o UserKnownHostsFile=/dev/null \
          -o StrictHostKeyChecking=no \
          -o User=root \
          $1
  '';
in
{
  imports = [
    "${inputs.self}/modules/home-manager/programs/emacs"
    "${inputs.self}/modules/home-manager/programs/email"
    "${inputs.self}/modules/home-manager/programs/fzf"
    "${inputs.self}/modules/home-manager/programs/hunspell"
    "${inputs.self}/modules/home-manager/programs/open-port"
    "${inputs.self}/modules/home-manager/programs/ssh"
    "${inputs.self}/modules/home-manager/programs/starship"
    "${inputs.self}/modules/home-manager/programs/tmux"
    "${inputs.self}/modules/home-manager/programs/yt-dlp"
    "${inputs.self}/modules/home-manager/services/desktop-base"
    git
    ./headless.nix
  ];

  home = {
    packages = [
      pkgs.exercism
      pkgs.gh # GitHub CLI for working on poetry2nix
      pkgs.git
      pkgs.nix-prefetch-github
      pkgs.nix-prefetch-scripts
      pkgs.nix-tree
      date-to-filename
      denote-rename
      rebuild
      unlock-luks
      update-file-dates
    ];
    shellAliases = {
      format-modules = "${pkgs.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
    };
  };
  services = {
    ssh-agent.enable = true;
  };
}

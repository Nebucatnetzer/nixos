{
  nixosConfig,
  inputs,
  pkgs,
  ...
}:
let
  denote-rename = pkgs.callPackage "${inputs.self}/pkgs/denote-rename" { };
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    runtimeInputs = [
      pkgs.nixos-rebuild-ng
    ];
    text = ''
      if ${pkgs.netcat}/bin/nc -vzw 2 ${nixosConfig.services.az-binary-cache-common.server} 2222; then
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
  imports = [ ./headless.nix ];

  home = {
    packages = [
      pkgs.exercism
      pkgs.gh # GitHub CLI for working on poetry2nix
      pkgs.git
      pkgs.nix-prefetch-github
      pkgs.nix-prefetch-scripts
      pkgs.nix-tree
      denote-rename
      rebuild
      unlock-luks
    ];
    shellAliases = {
      format-modules = "${pkgs.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
    };
  };

  programs = {
    az-emacs.enable = true;
    az-email.enable = true;
    az-fzf.enable = true;
    az-git.enable = true;
    az-hunspell.enable = true;
    az-open-port.enable = true;
    az-ssh.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
    az-yt-dlp.enable = true;
  };

  services = {
    az-desktop-base.enable = true;
    ssh-agent.enable = true;
  };
}

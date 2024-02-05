{ inputs, pkgs, ... }:
let
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
  imports = [ "${inputs.self}/home-manager/profiles/headless.nix" ];

  home = {
    packages = with pkgs; [
      docker-compose
      exercism
      git
      unlock-luks
    ];
    shellAliases = {
      format-modules = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
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
  };
}

{ inputs, pkgs, ... }:
{
  imports = [ "${inputs.self}/home-manager/profiles/headless.nix" ];

  home = {
    packages = with pkgs; [
      docker-compose
      exercism
      git
    ];
    shellAliases = {
      unlock-luks = "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o User=root";
      format-modules = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
    };
  };

  programs = {
    az-emacs.enable = true;
    az-git.enable = true;
    az-hunspell.enable = true;
    az-open-port.enable = true;
    az-ssh.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
  };
}

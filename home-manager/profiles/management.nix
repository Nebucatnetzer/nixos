{ inputs, pkgs, ... }:
{
  imports = [ "${inputs.self}/home-manager/modules" ];

  home = {
    packages = with pkgs; [
      docker-compose
      exercism
      nodePackages.prettier # formatting files
      xclip
    ];
    shellAliases = {
      unlock-luks = "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o User=root";
    };
  };

  programs = {
    az-emacs.enable = true;
    az-hunspell.enable = true;
    az-open-port.enable = true;
    az-ssh.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
  };
  systemd.user.startServices = "sd-switch";
}

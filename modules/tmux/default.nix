{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    extraConfig = ''
      set -g mouse on
    '';
  };
}

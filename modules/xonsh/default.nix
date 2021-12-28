# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:
{
  programs.xonsh = {
    enable = true;
    config = ''
      aliases['management-server'] = "mosh andreas@10.7.89.106 tmux a"
      aliases['nix-generations'] = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system"
      aliases['rebuild'] = "sudo nixos-rebuild -j auto switch"
      aliases['find-garbage'] = "ls -l /nix/var/nix/gcroots/auto/ | sort"
    '';
  };
  users.users.andreas = {
    shell = pkgs.xonsh;
  };
}


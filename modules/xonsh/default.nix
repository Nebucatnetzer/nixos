{ pkgs, ... }:
let
  username = import ../../username.nix;
in
{
  programs.xonsh = {
    enable = true;
    config = ''
      aliases['management-server'] = "mosh ${username}@10.7.89.106 tmux a"
      aliases['nix-generations'] = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system"
      aliases['rebuild'] = "sudo nixos-rebuild -j auto switch"
      aliases['find-garbage'] = "ls -l /nix/var/nix/gcroots/auto/ | sort"
      aliases['format-module'] = "nixpkgs-fmt **/*.nix"
    '';
  };
  users.users.${username} = {
    shell = pkgs.xonsh;
  };
}


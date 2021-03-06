{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    git
    highlight
    htop
    killall
    ncdu
    nixpkgs-fmt
    ranger
    tree
    unzip
    vim
    wget
  ];
  environment.shellAliases = {
    format-modules = "nixpkgs-fmt **/*.nix";
    nix-generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    rebuild = ''
      rm -rf ~/.config/qtile/__pycache__ &&
      rm -f ~/.emacs.d/loader.el &&
      nixos-rebuild -j auto switch --use-remote-sudo
    '';
    find-garbage = "ls -l /nix/var/nix/gcroots/auto/ | sort";
    vm = "vim";
  };
}


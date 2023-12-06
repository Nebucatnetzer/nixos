{ config, inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home = {
    activation.report-changes = config.lib.dag.entryAnywhere ''
      ${pkgs.nix}/bin/nix store diff-closures $oldGenPath $newGenPath || true
    '';
    sessionPath = [ "$HOME/node_modules/.bin" ];
    sessionVariables = {
      NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
      PATH = "$PATH:$HOME/.local/bin";
    };
    packages = with pkgs; [
      keychain
      nixpkgs-fmt
      mosh
    ];
  };
  nix = {
    package = pkgs.nix;
    settings = {
      connect-timeout = 5;
      log-lines = 25;
      experimental-features = "nix-command flakes";
      fallback = true;
      warn-dirty = false;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
    };
  };

  programs = {
    az-ansible.enable = true;
    az-emacs.enable = true;
    az-git = {
      userEmail = "zweili@contria.com";
    };
    az-tmux.enable = true;
    az-vagrant-wsl.enable = true;
    bash = {
      bashrcExtra = ''
        . /home/zweili/.nix-profile/etc/profile.d/nix.sh
      '';
      shellAliases = {
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
        work-vm = ''ssh andreas@localhost -p 2222 -t "$@" "tmux new -A -s 0"'';
        hm-rebuild = "home-manager switch";
      };
    };
  };
  systemd.user.startServices = "suggest";
}



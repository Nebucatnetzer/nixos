{ config, pkgs, ... }:
let
  hm-rebuild = pkgs.writeShellApplication {
    name = "hm-rebuild";
    runtimeInputs = [ ];
    text = ''
      home-manager switch
    '';
  };
in
{
  imports = [ ./headless.nix ];

  fonts.fontconfig.enable = true;

  home = {
    activation.report-changes = config.lib.dag.entryAnywhere ''
      ${pkgs.nix}/bin/nix store diff-closures $oldGenPath $newGenPath || true
    '';
    packages = [
      pkgs.bottom
      pkgs.gyre-fonts
      pkgs.highlight
      hm-rebuild
      pkgs.keychain
      pkgs.killall
      pkgs.mosh
      pkgs.ncdu
      pkgs.nix-tree
      pkgs.nmon
      pkgs.tree
      pkgs.unzip
      pkgs.wget
    ];
    sessionPath = [ "$HOME/.local/share/node_modules/bin" ];
    sessionVariables = {
      NIX_PATH = "nixpkgs=${pkgs.path}";
      NPM_CONFIG_PREFIX = "$HOME/.local/share/node_modules";
      PATH = "$PATH:$HOME/.local/bin";
    };
  };

  nix = {
    package = pkgs.nix;
    settings = {
      trusted-users = [
        "root"
        "@sudo"
      ];
      connect-timeout = 5;
      log-lines = 25;
      experimental-features = "nix-command flakes repl-flake";
      fallback = true;
      netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
      warn-dirty = false;
      secret-key-files = "${config.home.homeDirectory}/.config/nix/cache-priv-key.pem";
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
        "ssh://nix-pull@co-srv-pcache1.contria.srv?priority=50"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "co-srv-pcache1.contria.srv:A0Ov/Y/AA9nM02zGR035RhI93qfyatXuTHzrFTBLXXc="
      ];
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  programs = {
    az-ansible.enable = true;
    az-emacs.enable = true;
    az-fzf.enable = true;
    az-git = {
      enable = true;
      userEmail = "zweili@contria.com";
    };
    az-hunspell.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  services = {
    ssh-agent.enable = true;
  };

  systemd.user.timers.nix-gc = {
    Timer = {
      Persistent = true;
    };
  };
}

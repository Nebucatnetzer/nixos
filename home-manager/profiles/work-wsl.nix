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
      NIX_PATH = "nixpkgs=${pkgs.path}";
      PATH = "$PATH:$HOME/.local/bin";
    };
    packages = with pkgs; [
      bottom
      gyre-fonts
      highlight
      hm-rebuild
      keychain
      killall
      mosh
      ncdu
      nix-tree
      nmon
      tree
      unzip
      wget
    ];
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
  systemd.user.timers.nix-gc = {
    Timer = {
      Persistent = true;
    };
  };

  fonts.fontconfig.enable = true;
  programs = {
    az-ansible.enable = true;
    az-copilot-cli.enable = true;
    az-emacs.enable = true;
    az-fzf.enable = true;
    az-git = {
      enable = true;
      userEmail = "zweili@contria.com";
    };
    az-hunspell.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
    bash = {
      shellAliases = {
        work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
        work-vm = ''ssh andreas@localhost -p 2222 -t "$@" "tmux new -A -s 0"'';
      };
    };
  };
  services = {
    az-attic-client.enable = true;
    ssh-agent.enable = true;
  };
}

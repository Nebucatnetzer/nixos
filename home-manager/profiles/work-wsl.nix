{
  config,
  inputs,
  pkgs,
  system,
  ...
}:
let
  hm-rebuild = pkgs.writeShellApplication {
    name = "hm-rebuild";
    runtimeInputs = [
      pkgs.nixos-rebuild
      inputs.attic.packages.${system}.attic-client
    ];
    text = ''
      home-manager switch
      attic push prod ${config.home.homeDirectory}/.local/state/home-manager/gcroots/current-home
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
      warn-dirty = false;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
        "https://cache.zweili.org/prod"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "prod:46pIZhqoueg1P4IPp8ciArCUgSXWJZAq63CwLTQN/uA="
      ];
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

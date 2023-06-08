{ inputs, pkgs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
    "${inputs.self}/home-manager/software/ansible"
    "${inputs.self}/home-manager/software/emacs"
    "${inputs.self}/home-manager/software/fzf"
    "${inputs.self}/home-manager/software/git"
    "${inputs.self}/home-manager/software/starship"
    "${inputs.self}/home-manager/software/vagrant-wsl"
    "${inputs.self}/home-manager/software/vim"
  ];

  programs.git.userEmail = "zweili@contria.com";
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home = {
    username = "zweili";
    sessionPath = [ "$HOME/node_modules/.bin" ];
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

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . /home/zweili/.nix-profile/etc/profile.d/nix.sh
    '';
    shellAliases = {
      work-management = "mosh --ssh='ssh -i ~/.ssh/zweili.key' zweili@10.49.0.100 -- tmux new -A -s 0";
    };
  };
}
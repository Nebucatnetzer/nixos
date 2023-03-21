{ custom, pkgs, ... }:
let
  unstable = import custom.inputs.nixpkgs-unstable { inherit system; };
in
{
  imports = [
    (import "${custom.inputs.self}/home-manager/common" {
      custom = { username = "zweili"; version = "22.11"; };
    })
    "${custom.inputs.self}/home-manager/software/ansible"
    "${custom.inputs.self}/home-manager/software/emacs"
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/vagrant-wsl"
    "${custom.inputs.self}/home-manager/software/vim"
  ];

  programs.git.userEmail = "zweili@contria.com";
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    keychain
    nixpkgs-fmt
    mosh
  ];
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
      . ~/.nixos/home-manager/configs/bash/work_wsl_bashrc
      . /home/zweili/.nix-profile/etc/profile.d/nix.sh
    '';
  };
}

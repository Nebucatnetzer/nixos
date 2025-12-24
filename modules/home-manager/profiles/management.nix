{
  inputs,
  pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs; };
  git = import "${inputs.self}/modules/home-manager/programs/git" { };
in
{
  imports = [
    "${inputs.self}/modules/home-manager/programs/emacs"
    "${inputs.self}/modules/home-manager/programs/email"
    "${inputs.self}/modules/home-manager/programs/fzf"
    "${inputs.self}/modules/home-manager/programs/hunspell"
    "${inputs.self}/modules/home-manager/programs/open-port"
    "${inputs.self}/modules/home-manager/programs/ssh"
    "${inputs.self}/modules/home-manager/programs/starship"
    "${inputs.self}/modules/home-manager/programs/tmux"
    "${inputs.self}/modules/home-manager/programs/yt-dlp"
    "${inputs.self}/modules/home-manager/services/desktop-base"
    git
    ./headless.nix
  ];

  home = {
    packages = [
      azPkgs.date-to-filename
      azPkgs.denote-rename
      azPkgs.rebuild
      azPkgs.unlock-luks
      azPkgs.update-file-dates
      pkgs.exercism
      pkgs.gh # GitHub CLI for working on poetry2nix
      pkgs.git
      pkgs.nix-prefetch-github
      pkgs.nix-prefetch-scripts
      pkgs.nix-tree
      pkgs.nps
      pkgs.termscp
    ];
    shellAliases = {
      format-modules = "${pkgs.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
    };
  };
  services = {
    ssh-agent.enable = true;
  };
}

{
  inputs,
  pkgs,
  ...
}:
let
  git = import "${inputs.self}/modules/home-manager/programs/git" { };
in
{
  imports = [
    "${inputs.self}/modules/home-manager/programs/claude"
    "${inputs.self}/modules/home-manager/programs/emacs"
    "${inputs.self}/modules/home-manager/programs/email"
    "${inputs.self}/modules/home-manager/programs/fzf"
    "${inputs.self}/modules/home-manager/programs/hunspell"
    "${inputs.self}/modules/home-manager/programs/open-port"
    "${inputs.self}/modules/home-manager/programs/ssh"
    "${inputs.self}/modules/home-manager/programs/starship"
    "${inputs.self}/modules/home-manager/programs/tmux"
    "${inputs.self}/modules/home-manager/programs/yt-dlp"
    git
    ./headless.nix
  ];

  home = {
    sessionVariables = {
      PATH = "$PATH:$HOME/.local/bin";
    };
  };
  programs = {
    emacs.package = pkgs.emacs-nox;
  };
}

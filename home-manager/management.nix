{ custom, system }: { pkgs, ... }:
let
  unstable = import custom.inputs.nixpkgs-unstable { inherit system; };
in
{
  imports = [
    (import "${custom.inputs.self}/home-manager/common" { inherit custom; })
    (import "${custom.inputs.self}/home-manager/software/emacs" { inherit unstable; })
    "${custom.inputs.self}/home-manager/software/fzf"
    "${custom.inputs.self}/home-manager/software/git"
    "${custom.inputs.self}/home-manager/software/ssh"
    "${custom.inputs.self}/home-manager/software/starship"
    "${custom.inputs.self}/home-manager/software/vim"
  ];

  home.packages = with pkgs; [
    exercism
    nodePackages.prettier # formatting files
    rclone
    xclip
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };

}

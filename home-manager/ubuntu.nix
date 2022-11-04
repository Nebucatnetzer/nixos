{ custom, inputs }: { ... }:
{
  imports = [
    (import "${inputs.self}/home-manager/common" { inherit custom; })
    "${inputs.self}/home-manager/software/git"
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

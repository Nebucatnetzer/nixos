{ self, pkgs, ... }:
{
  imports = [
    (import ./common { inherit self; })
    ./software/git
    ./software/vim
  ];

  programs.git.userEmail = "andreas@zweili.ch";

  programs.bash = {
    enable = true;
    shellAliases = {
      management-server = "mosh --ssh='ssh -p 22' andreas@10.7.89.106 tmux a";
    };
  };

}

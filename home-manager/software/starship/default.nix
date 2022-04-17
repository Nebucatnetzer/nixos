{ lib, ... }:
{
  programs.starship = {
    settings = {
      add_newline = false;
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$directory"
        "$nix_shell"
        "$git_branch"
        "$git_status"
        "$character"
      ];
      nix_shell = {
        format = "$symbol";
      };
    };
    enable = true;
  };
}

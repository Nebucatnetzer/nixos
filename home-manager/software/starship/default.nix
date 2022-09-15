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
        "$python"
        "$git_branch"
        "$git_status"
        "$character"
      ];
      nix_shell = {
        format = "$symbol";
      };
      python = {
        format = "[$\{symbol\}($virtualenv) ]($style)";
      };
    };
    enable = true;
  };
}

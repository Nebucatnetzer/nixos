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
      # The default symbol has U+FE0F. tmux draws it 2 columns wide, bash counts 1.
      nix_shell = {
        symbol = "❄";
      };
      python = {
        format = "[\${symbol}($virtualenv) ]($style)";
      };
    };
    enable = true;
  };
}

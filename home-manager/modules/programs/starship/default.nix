{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-starship;
in
{
  options = {
    programs.az-starship.enable = lib.mkEnableOption "My configuration for Starship";
  };

  config = lib.mkIf cfg.enable {
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
        python = {
          format = "[\${symbol}($virtualenv) ]($style)";
        };
      };
      enable = true;
    };
  };
}

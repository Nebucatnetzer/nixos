{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-hunspell;
in
{
  options = {
    programs.az-hunspell.enable = lib.mkEnableOption "Add dictionaries";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.hunspell
      pkgs.hunspellDicts.en_GB-ise
      pkgs.hunspellDicts.de_CH
    ];
  };
}

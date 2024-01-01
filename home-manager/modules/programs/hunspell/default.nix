{ config, lib, pkgs, ... }:
let cfg = config.programs.az-hunspell;
in {
  options = {
    programs.az-hunspell.enable = lib.mkEnableOption "Add dictionaries";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      hunspell
      hunspellDicts.en_GB-ise
      hunspellDicts.de_CH
    ];
  };
}

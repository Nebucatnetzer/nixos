{ pkgs, ... }:
{
  home.packages = [
    pkgs.hunspell
    pkgs.hunspellDicts.en_GB-ise
    pkgs.hunspellDicts.de_CH
    pkgs.hyphenDicts.de-ch
    pkgs.hyphenDicts.en_US
  ];
}

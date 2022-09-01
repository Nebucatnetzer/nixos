{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    hunspell
    hunspellDicts.en_GB-ise
    hunspellDicts.de_CH
  ];
}

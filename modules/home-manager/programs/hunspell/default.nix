{ pkgs, ... }:
{
  home.packages = [
    # withDicts sets DICPATH inside the wrapper, so hunspell finds the
    # dictionaries also when $PATH lacks the profile (Emacs daemon under systemd).
    (pkgs.hunspell.withDicts (dicts: [
      dicts.en_GB-ise
      dicts.de_CH
    ]))
    pkgs.hyphenDicts.de-ch
    pkgs.hyphenDicts.en_GB
  ];
  home.sessionVariables.DICTIONARY = "en_GB";
}

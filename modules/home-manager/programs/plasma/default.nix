{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  plasma-config-dump = pkgs.callPackage ./plasma-dump.nix { inherit rc2nix; };
  rc2nix = inputs.plasma-manager.packages.${pkgs.stdenv.hostPlatform.system}.rc2nix;
  # plasma-manager always writes these keys, and rc2nix dumps them too.
  # Both definitions must be equal, so the module gets the dumped values.
  webShortcuts = (import ./config.nix).programs.plasma.configFile.kuriikwsfilterrc.General;
in
{
  # plasma-config-dump generates config.nix. Do not edit it by hand.
  imports = [ ./config.nix ];
  home.packages = [ plasma-config-dump ];
  programs.plasma = {
    enable = true;
    # Reset every setting that is not declared here. Run plasma-config-dump
    # before a switch, or the switch discards GUI changes.
    overrideConfig = true;
    # rc2nix does not read these files. Without the exclude, overrideConfig resets
    # them on every switch and plasma-config-dump cannot save their settings.
    resetFilesExclude = [
      "auroraerc"
      "KDE/Sonnet.conf"
      "kde.org/ghostwriter.conf"
      "kded6rc"
      "plasma_calendar_alternatecalendar"
      "plasma_calendar_astronomicalevents"
      "plasma_calendar_holiday_regions"
      "plasmashellrc"
      "powerdevilrc"
    ];
    searchPlugins.webSearchKeywords = {
      enable = webShortcuts.EnableWebShortcuts;
      default = webShortcuts.DefaultWebShortcut;
      delimiter = webShortcuts.KeywordDelimiter;
      preferred = lib.splitString "," webShortcuts.PreferredWebShortcuts;
      usePreferredOnly = webShortcuts.UsePreferredWebShortcutsOnly;
    };
  };
}

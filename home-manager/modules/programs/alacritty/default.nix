{ config, lib, ... }:
let
  cfg = config.programs.az-alacritty;
in
{
  options = {
    programs.az-alacritty.enable = lib.mkEnableOption "Enable alacritty terminal.";
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        env.TERM = "xterm-256color";
        window.opacity = 0.8;
        colors = {
          primary = {
            background = "#fdf6e3";
            foreground = "#657b83";
          };
          cursor = {
            text = "#fdf6e3";
            cursor = "#657b83";
          };
          normal = {
            black = "#073642";
            red = "#dc322f";
            green = "#859900";
            yellow = "#b58900";
            blue = "#268bd2";
            magenta = "#d33682";
            cyan = "#2aa198";
            white = "#eee8d5";
          };
          bright = {
            black = "#002b36";
            red = "#cb4b16";
            green = "#586e75";
            yellow = "#657b83";
            blue = "#839496";
            magenta = "#6c71c4";
            cyan = "#93a1a1";
            white = "#fdf6e3";
          };
        };
        font = {
          normal = {
            family = "Source Code Pro";
            style = "Regular";
          };
          bold = {
            family = "Source Code Pro";
            style = "Bold";
          };
          italic = {
            family = "Source Code Pro";
            style = "Italic";
          };
          bold_italic = {
            family = "Source Code Pro";
            style = "Bold Italic";
          };
          size = 14;
        };
      };
    };
  };
}

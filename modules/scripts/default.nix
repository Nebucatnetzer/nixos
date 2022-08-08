{ pkgs, ... }:
let
  remove-special-characters = pkgs.writeScriptBin
    "remove-special-characters"
    "${builtins.readFile ./remove_special_characters.sh}";

  compress-pdf = pkgs.writeScriptBin "compress-pdf" ''
    ${pkgs.ghostscript}/bin/gs -sDEVICE=pdfwrite \
        -dCompatibilityLevel=1.5 \
        -dNOPAUSE \
        -dQUIET \
        -dBATCH \
        -sOutputFile=compressed_$1 $1'';

in
{
  environment.systemPackages = [
    remove-special-characters
    compress-pdf
  ];
}


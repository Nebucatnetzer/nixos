{ pkgs, ... }:
let
  compress-pdf = pkgs.writeShellScriptBin "compress-pdf" ''
    ${pkgs.ghostscript}/bin/gs -sDEVICE=pdfwrite \
        -dCompatibilityLevel=1.5 \
        -dNOPAUSE \
        -dQUIET \
        -dBATCH \
        -sOutputFile=compressed_$1 $1'';

  files-to-lowercase = pkgs.writeScriptBin "files-to-lowercase"
    "${builtins.readFile ./files-to-lowercase.sh}";

  heif-to-jpeg = pkgs.writeShellScriptBin "heif-to-jpeg" ''
    for f in *.heic
    do
    echo "Working on file $f"
    ${pkgs.libheif}/bin/heif-convert $f $f.jpg
    done'';

  remove-special-characters = pkgs.writeScriptBin
    "remove-special-characters"
    "${builtins.readFile ./remove_special_characters.sh}";
in
{
  environment.systemPackages = [
    compress-pdf
    files-to-lowercase
    heif-to-jpeg
    remove-special-characters
  ];
}


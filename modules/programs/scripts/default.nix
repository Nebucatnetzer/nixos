{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-scripts;
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

  replace-listings = pkgs.writeScriptBin
    "replace-listings"
    "${builtins.readFile ./replace-listings.sh}";

  thumbnails = pkgs.writeShellScriptBin "thumbnails" ''
    for d in $1/*; do
      ${pkgs.ffmpeg}/bin/ffmpeg -i "$d" -t 2 -r 0.5 "$d".jpg
    done'';
in
{
  options = {
    programs.az-scripts.enable = lib.mkEnableOption "Enable scripts";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      compress-pdf
      files-to-lowercase
      heif-to-jpeg
      remove-special-characters
      replace-listings
      thumbnails
    ];
  };

}


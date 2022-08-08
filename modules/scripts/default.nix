{ pkgs, ... }:
let
  remove-special-characters = pkgs.writeScriptBin
    "remove-special-characters"
    "${builtins.readFile ./remove_special_characters.sh}";

in
{
  environment.systemPackages = [ remove-special-characters ];
}

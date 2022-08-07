{ pkgs, ... }:
let
  remove-special-characters = pkgs.writeShellScriptBin "${builtins.readFile ./remove_special_characters.sh}";
in
{
  environment.systemPackages = [ remove-special-characters ];
}

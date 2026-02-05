{ mpv, writeShellScriptBin }:
writeShellScriptBin "az-media" ''
  videos="videos"
  directory="''${1:-videos}"
  for i in $(seq 1 4);
  do
      ${mpv}/bin/mpv --shuffle --mute=yes "/run/media/andreas/various/$directory/" &
  done
''

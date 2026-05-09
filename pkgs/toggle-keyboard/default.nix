{
  lib,
  evtest,
  procps,
  writeShellApplication,
}:
writeShellApplication {
  name = "toggle-keyboard";
  runtimeInputs = [
    evtest
    procps
  ];
  meta = {
    description = "Toggle an external keyboard on and off via evtest";
    license = lib.licenses.gpl3Plus;
    mainProgram = "toggle-keyboard";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./toggle-keyboard.sh;
}

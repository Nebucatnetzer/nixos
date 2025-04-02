{
  evtest,
  procps, # provides pgrep and pkill
  writeShellApplication,
}:
writeShellApplication {
  name = "toggle-keyboard";
  runtimeInputs = [
    evtest
    procps
  ];
  text = builtins.readFile ./toggle-keyboard.sh;
}

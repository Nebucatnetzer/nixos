{
  bash,
  coreutils,
  gawk,
  gnugrep,
  writeShellApplication,
  xorg,
}:
writeShellApplication {
  name = "toggle-keyboard";
  runtimeInputs = [
    bash
    coreutils
    gawk
    gnugrep
    xorg.xinput
  ];
  text = builtins.readFile ./toggle-keyboard.sh;
}

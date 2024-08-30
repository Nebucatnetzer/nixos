{
  bash,
  coreutils,
  gawk,
  gnugrep,
  rsync,
  writeShellApplication,
}:
writeShellApplication {
  name = "dap-sync";
  runtimeInputs = [
    bash
    coreutils
    gawk
    gnugrep
    rsync
  ];
  excludeShellChecks = [ "SC2295" ];
  text = builtins.readFile ./dap-sync.sh;
}

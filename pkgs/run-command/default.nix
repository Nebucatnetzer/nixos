{
  writers,
}:
writers.writePython3Bin "run-command" {
  flakeIgnore = [ "E501" ];
} (builtins.readFile ./run_command.py)

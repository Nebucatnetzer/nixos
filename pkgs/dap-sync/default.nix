{
  rsync,
  writers,
  writeShellApplication,
}:
let
  dap-sync-python = writers.writePython3Bin "dap-sync-python" {
    flakeIgnore = [
      "E501"
      "E203"
    ];
  } (builtins.readFile ./dap_sync.py);
in
writeShellApplication {
  name = "dap-sync";
  runtimeInputs = [
    rsync
    dap-sync-python
  ];
  text = ''
    dap-sync-python "$@"
  '';
}

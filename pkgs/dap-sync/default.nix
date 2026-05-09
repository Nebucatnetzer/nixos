{
  lib,
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
  meta = {
    description = "Sync DAP (Digital Audio Player) content to and from the external SSD";
    license = lib.licenses.gpl3Plus;
    mainProgram = "dap-sync";
    platforms = lib.platforms.linux;
  };
  text = ''
    dap-sync-python "$@"
  '';
}

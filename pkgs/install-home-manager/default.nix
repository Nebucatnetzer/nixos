{
  lib,
  home-manager,
  nix-output-monitor,
  writeShellApplication,
}:
writeShellApplication {
  name = "install-home-manager";
  runtimeInputs = [
    home-manager
    nix-output-monitor
  ];
  meta = {
    description = "Install home-manager for first-time setup";
    license = lib.licenses.gpl3Plus;
    mainProgram = "install-home-manager";
    platforms = lib.platforms.linux;
  };
  text = builtins.readFile ./install_home_manager.sh;
}

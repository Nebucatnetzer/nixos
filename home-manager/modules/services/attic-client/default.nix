{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  system = pkgs.system;
  cfg = config.services.az-attic-client;
  atticWatcher = pkgs.writeShellScript "attic-watcher" ''
    ${inputs.attic.packages.${system}.attic-client}/bin/attic watch-store prod
  '';
in
{
  options = {
    services.az-attic-client = {
      enable = lib.mkEnableOption "Enable a service to watch the nix store and upload new paths to attic";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ inputs.attic.packages.${system}.attic-client ];
    systemd.user.services.az-attic-client = {
      Unit = {
        Description = "Watch the nix store and upload new paths to attic";
        After = [ "network.target" ];
      };
      Install = {
        WantedBy = [ "multi-user.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${atticWatcher}";
        Restart = "always";
      };
    };
  };
}

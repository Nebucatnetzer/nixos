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
    environment.systemPackages = [ inputs.attic.packages.${system}.attic-client ];
    # sytemd service
    systemd.services.az-attic-client = {
      description = "Watch the nix store and upload new paths to attic";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${atticWatcher}";
        Restart = "always";
        RestartSec = "5";
        User = config.az-username;
        Group = "users";
      };
    };
  };
}

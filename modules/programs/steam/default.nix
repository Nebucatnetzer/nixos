{ config, lib, ... }:
let cfg = config.programs.az-steam;
in {
  options = { programs.az-steam.enable = lib.mkEnableOption "Enable Steam"; };

  config = lib.mkIf cfg.enable {
    programs.steam.enable = true;
    hardware.steam-hardware.enable = true;
    networking.firewall = {
      allowedTCPPorts = [ 27036 ];
      allowedUDPPorts = [ 27031 ];
    };
    home-manager.users.${config.az-username} = {
      home.file.".local/share/applications/steam.desktop".source =
        ./steam.desktop;
    };
  };
}

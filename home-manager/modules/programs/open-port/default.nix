{ config, lib, ... }:
let
  cfg = config.programs.az-open-port;
in
{
  options = {
    programs.az-open-port.enable = lib.mkEnableOption "Two functions to quickly open a port in iptables.";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      bash = {
        bashrcExtra = ''
          open-port() {
            local port=$1
            sudo iptables -A INPUT -p tcp --dport $port -j ACCEPT
          }

          close-port() {
            local port=$1
            sudo iptables -D INPUT -p tcp --dport $port -j ACCEPT
          }
        '';
      };
    };
  };
}

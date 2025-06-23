{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-librenms-agent;
  librenms-agent = pkgs.stdenv.mkDerivation {
    pname = "librenms-agent";
    version = "0.1";
    src = pkgs.fetchFromGitHub {
      owner = "librenms";
      repo = "librenms-agent";
      # upstream provides no git tags
      rev = "def5f830b3672526c4353a92a1804485f645733c";
      hash = "sha256-rzwSYJlor1LMGWDP4i/UYmuRccH0NflMC/fXDNd7QXA=";
    };

    installPhase = ''
      runHook preInstall
      install -D -m 0750 check_mk_agent -t $out/bin/
      runHook postInstall
    '';
  };
in
{
  options.services.az-librenms-agent = {
    enable = lib.mkEnableOption "LibreNMS agent";

    port = lib.mkOption {
      type = lib.types.port;
      default = 6556;
      description = "Port where the LibreNMS Agent will listen.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Open port for LibreNMS Agent.
      '';
    };

    ipAddressAllow = lib.mkOption {
      default = [ "10.7.89.153/24" ];
      example = [ "192.168.1.0/24" ];
      type = lib.types.listOf lib.types.str;
      description = ''
        Allows access to the LibreNMS Agent only for the given addresses.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    systemd.sockets.librenms-agent = {
      description = "Check_MK LibreNMS Agent Socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = toString cfg.port;
        Accept = "yes";
      };
    };

    systemd.services."librenms-agent@" = {
      description = "Check_MK LibreNMS Agent Service";
      after = [
        "network.target"
        "librenms-agent.socket"
      ];
      requires = [ "librenms-agent.socket" ];
      serviceConfig = {
        ExecStart = lib.getExe librenms-agent;
        StandardOutput = "socket";
        IPAddressDeny = "any";
        IPAddressAllow = cfg.ipAddressAllow;
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
  };

}

{
  port ? 6556,
  ipAddressAllow ? [ "10.7.89.153/24" ],
}:
{
  config,
  lib,
  pkgs,
  ...
}:
let
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

  systemd.sockets.librenms-agent = {
    description = "Check_MK LibreNMS Agent Socket";
    wantedBy = [ "sockets.target" ];
    socketConfig = {
      ListenStream = toString port;
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
      IPAddressAllow = ipAddressAllow;
    };
  };

  networking.firewall.allowedTCPPorts = config.networking.firewall.allowedTCPPorts ++ [ port ];

}

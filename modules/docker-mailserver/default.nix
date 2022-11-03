{ inputs }: { config, pkgs, ... }:
let
  mailserver-setup = (pkgs.writeScriptBin "mailserver-setup"
    "${builtins.readFile (pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/docker-mailserver/docker-mailserver/v11.2.0/setup.sh";
      sha256 = "sha256-V4NFapoU3thbPzhSX5DGR3cZAW1kCYZpAKsFeSjMGPY=";
    })
      }").overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
{
  environment.systemPackages = [
    mailserver-setup
  ];
  virtualisation.oci-containers = {
    backend = "docker";
    containers."mailserver" = {
      image = "docker.io/mailserver/docker-mailserver:11.2.0";
      autoStart = true;
      environmentFiles = [
        "${inputs.self}/modules/docker-mailserver/mailserver.env"
      ];
      ports = [
        "25:25"
        "143:143"
        "465:465"
        "587:587"
        "993:993"
      ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "/var/lib/acme/mail.zweili.org:/etc/letsencrypt/live/mail.zweili.org:ro"
        "${inputs.self}/modules/docker-mailserver/sa-learn:/etc/cron.d/sa-learn"
      ];
      extraOptions = [
        ''--mount=type=volume,source=maildata,target=/var/mail,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/maildata,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=mailstate,target=/var/mail-state,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/mailstate,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=maillogs,target=/var/log/mail,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/maillogs,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=config,target=/tmp/docker-mailserver,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/config,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--add-host=host.docker.internal:host-gateway"
        "--cap-add=NET_ADMIN"
        "--cap-add=SYS_PTRACE"
      ];
    };
  };
}

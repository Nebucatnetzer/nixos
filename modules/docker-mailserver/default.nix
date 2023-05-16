{ custom }: { config, pkgs, ... }:
let
  version = "12.1.0";
  mailserver-setup = (pkgs.writeScriptBin "mailserver-setup"
    "${builtins.readFile (pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/docker-mailserver/docker-mailserver/v${version}/setup.sh";
      sha256 = "sha256-i5RaupuYo3Bh99/FKbxXKFITO3Ko3LrWPvTD6xPBXzY";
    })
      }").overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
{
  imports = [
    (import "${custom.inputs.self}/modules/telegram-notifications" { inherit custom; })
  ];

  age.secrets.dkim2liCh = {
    file = "${custom.inputs.self}/scrts/dkim_2li.ch.age";
    mode = "600";
    owner = "113";
    group = "115";
  };
  age.secrets.dkimZweiliCh = {
    file = "${custom.inputs.self}/scrts/dkim_zweili.ch.age";
    mode = "600";
    owner = "113";
    group = "115";
  };

  environment.etc = {
    "dkim/2li.ch.private" = {
      enable = true;
      source = config.age.secrets.dkim2liCh.path;
    };
    "dkim/zweili.ch.private" = {
      enable = true;
      source = config.age.secrets.dkimZweiliCh.path;
    };
  };

  environment.systemPackages = [
    mailserver-setup
  ];
  virtualisation.oci-containers = {
    backend = "docker";
    containers."mailserver" = {
      # https://hub.docker.com/r/mailserver/docker-mailserver/tags
      image = "docker.io/mailserver/docker-mailserver:${version}";
      autoStart = true;
      environmentFiles = [
        ./mailserver.env
      ];
      ports = [
        "25:25"
        "143:143"
        "465:465"
        "587:587"
        "993:993"
        "11334:11334"
      ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "/etc/dkim:/etc/dkim:ro"
        "/etc/static:/etc/static:ro"
        "/run/agenix:/run/agenix:ro"
        "/var/lib/acme/mail.zweili.org:/etc/letsencrypt/live/mail.zweili.org:ro"
        "/var/lib/redis:/var/lib/redis"
      ];
      extraOptions = [
        ''--mount=type=volume,source=maildata,target=/var/mail,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/maildata,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=mailstate,target=/var/mail-state,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/mailstate,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=maillogs,target=/var/log/mail,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/maillogs,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        ''--mount=type=volume,source=config,target=/tmp/docker-mailserver,volume-driver=local,volume-opt=type=nfs,volume-opt=device=:/server_data/docker-mailserver/config,"volume-opt=o=addr=10.7.89.108,rw,nfsvers=4.0,nolock,hard,noatime"''
        "--add-host=host.docker.internal:host-gateway"
        "--cap-add=NET_ADMIN"
        "--cap-add=SYS_PTRACE"
        "--log-opt=tag='mailserver'"
      ];
    };
  };
}

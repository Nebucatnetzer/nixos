{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-mailserver;
  version = "13.3.1";
  mailserver-setup =
    (pkgs.writeScriptBin "mailserver-setup" "${builtins.readFile (
      pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/docker-mailserver/docker-mailserver/v${version}/setup.sh";
        sha256 = "sha256-HMT790mp5ADdNYaOLUJfHJq9LWI0OPilTabAhogVXnc=";
      }
    )}"
    ).overrideAttrs
      (
        old: {
          buildCommand = ''
            ${old.buildCommand}
             patchShebangs $out'';
        }
      );
  volumePath = "/mnt/server-data/docker-mailserver";
in
{
  options = {
    services.az-mailserver.enable = lib.mkEnableOption "Enable docker-mailserver";
  };

  config = lib.mkIf cfg.enable {
    services.az-telegram-notifications.enable = true;

    age.secrets.dkim2liCh = {
      file = "${inputs.self}/scrts/dkim_2li.ch.age";
      mode = "600";
      owner = "113";
      group = "115";
    };
    age.secrets.dkimZweiliCh = {
      file = "${inputs.self}/scrts/dkim_zweili.ch.age";
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

    environment.systemPackages = [ mailserver-setup ];

    fileSystems."${volumePath}" = {
      device = "10.7.89.108:server_data/docker-mailserver";
      fsType = "nfs";
      options = [
        "hard"
        "nfsvers=4.0"
        "noatime"
        "nolock"
        "rw"
      ];
    };
    services.az-docker.enable = true;

    virtualisation.oci-containers = {
      backend = "docker";
      containers."mailserver" = {
        # https://hub.docker.com/r/mailserver/docker-mailserver/tags
        image = "docker.io/mailserver/docker-mailserver:${version}";
        autoStart = true;
        environmentFiles = [ ./mailserver.env ];
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
          "${volumePath}/maildata:/var/mail"
          "${volumePath}/mailstate:/var/mail-state"
          "${volumePath}/maillogs:/var/log/mail"
          "${volumePath}/config:/tmp/docker-mailserver"
        ];
        extraOptions = [
          "--add-host=host.docker.internal:host-gateway"
          "--cap-add=NET_ADMIN"
          "--log-opt=tag='mailserver'"
        ];
      };
    };
  };
}

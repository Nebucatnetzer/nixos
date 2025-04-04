{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-mailserver;
  version = "14.0.0";
  mailserver-setup =
    (pkgs.writeScriptBin "mailserver-setup" "${builtins.readFile (
      pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/docker-mailserver/docker-mailserver/v${version}/setup.sh";
        sha256 = "sha256-HMT790mp5ADdNYaOLUJfHJq9LWI0OPilTabAhogVXnc=";
      }
    )}").overrideAttrs
      (old: {
        buildCommand = ''
          ${old.buildCommand}
           patchShebangs $out'';
      });
  rspam-train = pkgs.writeShellApplication {
    name = "rspam-train";
    runtimeInputs = [ pkgs.docker ];
    text = ''
      docker exec mailserver bash -c "rspamc learn_ham /var/mail/*/*/.Archive"
      docker exec mailserver bash -c "rspamc learn_spam /var/mail/*/*/.Junk"
    '';
  };
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
      owner = "111";
      group = "114";
    };
    age.secrets.dkimZweiliCh = {
      file = "${inputs.self}/scrts/dkim_zweili.ch.age";
      mode = "600";
      owner = "111";
      group = "114";
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
      rspam-train
    ];

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

    systemd.services."nixos-upgrade" = {
      serviceConfig = {
        ExecStartPost = "${pkgs.systemd}/bin/systemctl restart docker-mailserver.service";
      };
    };
    virtualisation.oci-containers = {
      backend = "docker";
      containers."mailserver" = {
        # https://hub.docker.com/r/mailserver/docker-mailserver/tags
        image = "docker.io/mailserver/docker-mailserver:${version}@sha256:01b82c6c1c64d5b73ba161c0107e5d7d038ebdce85eb9ed084f043e9c2a7943b";
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
          "/run/agenix.d:/run/agenix.d:ro"
          "/var/lib/acme/mail.zweili.org:/etc/letsencrypt/live/mail.zweili.org:ro"
          "${volumePath}/maildata:/var/mail"
          "${volumePath}/maillogs:/var/log/mail"
          "/var/lib/mailserver/mailstate:/var/mail-state"
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

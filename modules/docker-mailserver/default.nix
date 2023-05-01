{ custom }: { config, pkgs, ... }:
let
  version = "12.0.0";
  mailserver-setup = (pkgs.writeScriptBin "mailserver-setup"
    "${builtins.readFile (pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/docker-mailserver/docker-mailserver/v${version}/setup.sh";
      sha256 = "sha256-G2el0HNuecB4Y136hIdwcINdkBq29nnTTk/iC9QQuHI=";
    })
      }").overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
{
  imports = [
    (import "${custom.inputs.self}/modules/telegram-notifications" { inherit custom; })
  ];
  environment.systemPackages = [
    mailserver-setup
  ];
  virtualisation.oci-containers = {
    backend = "docker";
    containers."mailserver" = {
      # https://hub.docker.com/r/mailserver/docker-mailserver/tags
      image = "docker.io/mailserver/docker-mailserver:{version}";
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
      ];
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "/var/lib/acme/mail.zweili.org:/etc/letsencrypt/live/mail.zweili.org:ro"
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
  systemd.timers."mailserver-sa-learn" = {
    wantedBy = [ "timers.target" ];
    partOf = [ "mailserver-sa-learn.service" ];
    timerConfig = {
      OnCalendar = "daily";
    };
  };

  systemd.services."mailserver-sa-learn" = {
    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      # learn spam
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --spam /var/mail/2li.ch/*/.Junk --dbpath /var/mail-state/lib-amavis/.spamassassin
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --spam /var/mail/zweili.ch/*/.Junk --dbpath /var/mail-state/lib-amavis/.spamassassin
      # ham: archive directories
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --ham /var/mail/2li.ch/*/.Archive* --dbpath /var/mail-state/lib-amavis/.spamassassin
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --ham /var/mail/zweili.ch/*/.Archive* --dbpath /var/mail-state/lib-amavis/.spamassassin
      # ham: inbox subdirectories
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --ham /var/mail/2li.ch/*/cur* --dbpath /var/mail-state/lib-amavis/.spamassassin
      ${pkgs.docker}/bin/docker exec mailserver sa-learn --ham /var/mail/zweili.ch/*/cur* --dbpath /var/mail-state/lib-amavis/.spamassassin
    '';
  };
}

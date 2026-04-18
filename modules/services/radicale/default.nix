{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  domain = "dav.zweili.org";
  port = 5232;
  authFile = config.age.secrets.radicaleHtpasswd.path;
  publicIcsLocationFile = config.age.secrets.radicalePublicIcsLocation.path;
  smtpPasswordFile = config.age.secrets.radicaleSmtpPassword.path;
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
  radicaleRightsFile = pkgs.writeText "radicale-rights" ''
    [root]
    user: .*
    collection:
    permissions: R

    [principal]
    user: .*
    collection: {user}
    permissions: RW

    [collections]
    user: .*
    collection: {user}/[^/]+
    permissions: rw
  '';
  radicaleStartScript = pkgs.writeShellScript "radicale-start-with-smtp-secret" ''
        set -eu

        smtp_password="$(${pkgs.coreutils}/bin/cat "${smtpPasswordFile}")"
        runtime_dir="''${RUNTIME_DIRECTORY:-/run/radicale}"
        config_file="$runtime_dir/config"
        rights_file="$runtime_dir/rights"

        ${pkgs.coreutils}/bin/mkdir -p "$runtime_dir"
        ${pkgs.coreutils}/bin/cp "${radicaleRightsFile}" "$rights_file"
        ${pkgs.coreutils}/bin/chmod 600 "$rights_file"

        ${pkgs.coreutils}/bin/cat > "$config_file" <<EOF
    [server]
    hosts = 127.0.0.1:${toString port}

    [auth]
    type = htpasswd
    htpasswd_filename = ${authFile}
    htpasswd_encryption = bcrypt

    [storage]
    filesystem_folder = /var/lib/radicale/collections

    [hook]
    type = email
    smtp_server = mail.infomaniak.com
    smtp_port = 465
    smtp_security = tls
    smtp_username = admin@zweili.ch
    smtp_password = $smtp_password
    from_email = andreas@zweili.ch

    [rights]
    type = from_file
    file = /run/radicale/rights
    EOF
        ${pkgs.coreutils}/bin/chmod 600 "$config_file"

        exec ${unstable.radicale}/bin/radicale -C "$config_file"
  '';
in
{
  imports = [
    "${inputs.self}/modules/services/nginx-acme-base"
  ];

  age.secrets.radicaleSmtpPassword = {
    file = "${inputs.self}/scrts/mail_password.age";
    mode = "440";
    owner = "root";
    group = "radicale";
  };
  age.secrets.radicaleHtpasswd = {
    file = "${inputs.self}/scrts/radicale_htpasswd.age";
    mode = "440";
    owner = "root";
    group = "radicale";
  };
  age.secrets.radicalePublicIcsLocation = {
    file = "${inputs.self}/scrts/radicale_public_ics_location.age";
    mode = "440";
    owner = "root";
    group = config.services.nginx.group;
  };

  services.radicale = {
    enable = true;
    package = unstable.radicale;
    settings = {
      server.hosts = [ "127.0.0.1:${toString port}" ];
      auth = {
        type = "htpasswd";
        htpasswd_filename = authFile;
        htpasswd_encryption = "bcrypt";
      };
      storage.filesystem_folder = "/var/lib/radicale/collections";
    };
  };
  systemd.services.radicale = {
    serviceConfig = {
      # Radicale does not support reading SMTP password from a file, so we
      # generate a runtime config in /run and inject the age-managed secret.
      ExecStart = lib.mkForce radicaleStartScript;
      RuntimeDirectory = "radicale";
      RuntimeDirectoryMode = "0750";
    };
  };

  services.nginx = {
    recommendedProxySettings = true;
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString port}/";
        extraConfig = ''
          proxy_set_header X-Forwarded-Proto https;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_pass_header Authorization;
        '';
      };
      extraConfig = ''
        include ${publicIcsLocationFile};
      '';
    };
  };
}

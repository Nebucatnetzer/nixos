{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.services.az-firefly;
  keyFile = config.age.secrets.fireflyKeyFile.path;
  mailPasswordFile = config.age.secrets.mailPasswordFile.path;
in
{
  options = {
    services.az-firefly.enable = lib.mkEnableOption "Enable my configuration for Firefly III.";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.fireflyKeyFile = {
      file = "${inputs.self}/scrts/firefly_key.age";
      mode = "600";
      owner = config.services.firefly-iii.user;
      group = config.services.firefly-iii.group;
    };
    age.secrets.mailPasswordFile = {
      file = "${inputs.self}/scrts/mail_password.age";
      mode = "600";
      owner = config.services.firefly-iii.user;
      group = config.services.firefly-iii.group;
    };
    services = {
      nginx = {
        recommendedOptimisation = true;
        recommendedTlsSettings = true;
        recommendedProxySettings = true;
        virtualHosts = {
          ${config.services.firefly-iii.virtualHost} = {
            enableACME = true;
            forceSSL = true;
          };
        };
      };
      firefly-iii = {
        enable = true;
        enableNginx = true;
        settings = {
          APP_ENV = "production";
          DB_CONNECTION = "sqlite";
          APP_KEY_FILE = keyFile;
          MAIL_MAILER = "smtp";
          MAIL_HOST = "mail.zweili.org";
          MAIL_PORT = 465;
          MAIL_FROM = "admin@zweili.ch";
          MAIL_USERNAME = "admin@zweili.ch";
          MAIL_PASSWORD_FILE = mailPasswordFile;
          MAIL_ENCRYPTION = "tls";
        };
        virtualHost = "firefly.zweili.org";
      };
    };
  };
}

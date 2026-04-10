{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  adminPasswordFile = config.age.secrets.adminPasswordFile.path;
  appSecretFile = config.age.secrets.appSecretFile.path;
  mailDsnFile = config.age.secrets.mailDsnFile.path;
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in
{
  age.secrets.adminPasswordFile = {
    file = "${inputs.self}/scrts/davis_admin_password.age";
    mode = "640";
    owner = "root";
    group = config.services.davis.group;
  };
  age.secrets.appSecretFile = {
    file = "${inputs.self}/scrts/davis_app_secret.age";
    mode = "640";
    owner = "root";
    group = config.services.davis.group;
  };
  age.secrets.mailDsnFile = {
    file = "${inputs.self}/scrts/davis_mail_dsn.age";
    mode = "640";
    owner = "root";
    group = config.services.davis.group;
  };

  services.davis = {
    adminLogin = "thedoctor";
    adminPasswordFile = adminPasswordFile;
    appSecretFile = appSecretFile;
    config = {
      APP_TIMEZONE = "Europe/Zurich";
      CALDAV_ENABLED = true;
      CARDDAV_ENABLED = true;
      LOG_FILE_PATH = lib.mkForce "/var/lib/davis/var/log/davis.log";
      PUBLIC_CALENDARS_ENABLED = true;
      WEBDAV_ENABLED = false;
    };
    enable = true;
    hostname = "${config.networking.hostName}.local";
    mail = {
      dsnFile = mailDsnFile;
      inviteFromAddress = "andreas@zweili.ch";
    };
    package = unstable.davis;
  };
}

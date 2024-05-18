{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-email;
in
{
  options = {
    programs.az-email.enable = lib.mkEnableOption "Configure everything required for sending emails.";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.personalEmailKey = {
      file = "${inputs.self}/scrts/personal_email.key.age";
      mode = "600";
    };

    accounts.email.accounts."personal" = {
      address = "andreas@zweili.ch";
      realName = "Andreas Zweili";
      userName = "andreas@zweili.ch";
      primary = true;
      passwordCommand = "cat /run/user/1000/agenix/personalEmailKey";
      aliases = [
        "andreas.zweili@gmail.com"
        "andreas@2li.ch"
      ];
      msmtp.enable = true;
      mu.enable = true;
      offlineimap = {
        enable = true;
        extraConfig = {
          account = {
            autorefresh = 15;
          };
          local = {
            sync_deletes = true;
          };
        };
      };
      imap = {
        host = "mail.zweili.org";
        port = 993;
        tls.enable = true;
      };
      smtp = {
        host = "mail.zweili.org";
        port = 465;
        tls.enable = true;
      };
    };
    programs.mu = {
      enable = true;
      package = pkgs.unstable.mu;
    };
    programs.offlineimap.enable = true;
    programs.msmtp.enable = true;
  };
}

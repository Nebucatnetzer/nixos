{ pkgs, ... }:
{
  accounts.email.accounts."personal" = {
    address = "andreas@zweili.ch";
    realName = "Andreas Zweili";
    userName = "andreas@zweili.ch";
    primary = true;
    passwordCommand = "cat /home/andreas/.nixos/secrets/passwords/personal_email.key";
    aliases = [
      "andreas.zweili@gmail.com"
      "andreas@2li.ch"
    ];
    mu.enable = true;
    offlineimap = {
      enable = true;
      extraConfig = {
        account = { autorefresh = 15; };
        local = { sync_deletes = true; };
      };
    };
    imap = {
      host = "mail.zweili.org";
      port = 993;
      tls.enable = true;
    };
    smtp = {
      host = "mail.zweili.org";
      port = 587;
      tls = {
        enable = true;
        useStartTls = true;
      };
    };
  };
  programs.mu.enable = true;
  programs.offlineimap.enable = true;
}

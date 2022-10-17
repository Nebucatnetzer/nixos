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
    msmtp.enable = true;
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
      port = 465;
      tls.enable = true;
    };
  };
  programs.mu.enable = true;
  programs.offlineimap.enable = true;
  programs.msmtp.enable = true;
}

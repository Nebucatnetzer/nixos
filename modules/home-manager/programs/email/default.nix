{
  inputs,
  pkgs,
  ...
}:
{
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
        local = {
          sync_deletes = true;
        };
      };
    };
    imap = {
      host = "mail.infomaniak.com";
      port = 993;
      tls.enable = true;
    };
    smtp = {
      host = "mail.infomaniak.com";
      port = 465;
      tls.enable = true;
    };
  };
  programs.mu = {
    enable = true;
    package = pkgs.mu;
  };
  programs.offlineimap.enable = true;
  programs.msmtp.enable = true;
}

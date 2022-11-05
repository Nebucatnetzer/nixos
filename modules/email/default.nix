{ custom }: { config, ... }:
{
  age.secrets.personalEmailKey =
    {
      file = "${custom.inputs.self}/scrts/personal_email.key.age";
      mode = "600";
      owner = custom.username;
      group = "users";
    };

  home-manager.users.${custom.username} = {
    accounts.email.accounts."personal" = {
      address = "andreas@zweili.ch";
      realName = "Andreas Zweili";
      userName = "andreas@zweili.ch";
      primary = true;
      passwordCommand = "cat ${config.age.secrets.personalEmailKey.path}";
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
  };
}

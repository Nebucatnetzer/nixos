{
  config,
  inputs,
  lib,
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

  # msmtpq from Emacs needs nc on PATH for its connection test.
  home.packages = [ pkgs.netcat-gnu ];

  # msmtpq reads this file. Quiet mode is required, because Emacs treats any output as a send error.
  home.file.".msmtpqrc".text = ''
    MSMTPQ_Q="$HOME/.local/state/msmtp/queue"
    MSMTPQ_LOG="$HOME/.local/state/msmtp/queue.log"
    EMAIL_CONN_TEST=n
    EMAIL_QUEUE_QUIET=t
  '';

  systemd.user.services.msmtp-queue-flush = {
    Unit.Description = "Send mail from the msmtp queue";
    Service = {
      Type = "oneshot";
      # msmtpq calls msmtp and nc from PATH. The password command needs cat.
      Environment = "PATH=${
        lib.makeBinPath [
          config.programs.msmtp.package
          pkgs.netcat-gnu
          pkgs.coreutils
        ]
      }";
      ExecStart = "${config.programs.msmtp.package}/bin/msmtp-queue -r";
    };
  };

  systemd.user.timers.msmtp-queue-flush = {
    Unit.Description = "Send queued mail every 5 minutes";
    Timer.OnCalendar = "*:0/5";
    Install.WantedBy = [ "timers.target" ];
  };
}

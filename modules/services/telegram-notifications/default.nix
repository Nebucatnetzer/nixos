{
  config,
  inputs,
  pkgs,
  ...
}:
let
  # Lives in its own file so other modules can call it directly instead of going through
  # the unit-status-telegram@ indirection.
  send-to-telegram = pkgs.callPackage ./send_to_telegram.nix {
    envFile = config.age.secrets.telegramNotifyEnv.path;
  };

  unit-status-telegram = pkgs.writeShellScript "unit-status-telegram" ''
    UNIT="$1"

    UNITSTATUS="$(systemctl status $UNIT)"
    ALERT="$(echo -e "\u26A0")"

    ${send-to-telegram}/bin/send-to-telegram "$ALERT Unit failed $UNIT $ALERT
    Status:
    $UNITSTATUS"'';
in
{
  age.secrets.telegramNotifyEnv = {
    file = "${inputs.self}/scrts/telegram_notify_env.age";
    mode = "644";
  };
  environment.systemPackages = [ send-to-telegram ];
  systemd.services."unit-status-telegram@" = {
    description = "Unit Status Telegram Service";
    unitConfig = {
      After = "network-online.target";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = "${unit-status-telegram} %i";
    };
  };
}

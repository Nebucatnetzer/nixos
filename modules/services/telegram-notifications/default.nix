{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-telegram-notifications;

  send-to-telegram = pkgs.writeShellScriptBin "send-to-telegram" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${config.age.secrets.telegramNotifyEnv.path} | ${pkgs.findutils}/bin/xargs)
    URL="https://api.telegram.org/bot$TELEGRAM_KEY/sendMessage"
    ${pkgs.curl}/bin/curl -s -d "chat_id=$CHAT_ID&disable_web_page_preview=1&text=$1" $URL > /dev/null'';

  unit-status-telegram = pkgs.writeShellScript "unit-status-telegram" ''
    UNIT="$1"

    UNITSTATUS="$(systemctl status $UNIT)"
    ALERT="$(echo -e "\u26A0")"

    ${send-to-telegram} "$ALERT Unit failed $UNIT $ALERT
    Status:
    $UNITSTATUS"'';
in
{
  options = {
    services.az-telegram-notifications.enable = lib.mkEnableOption "Enable Telegram Notifications";
  };

  config = lib.mkIf cfg.enable {
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
        ExecStart = "${unit-status-telegram} %I";
      };
    };
  };
}

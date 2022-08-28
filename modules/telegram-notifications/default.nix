{ custom, pkgs, ... }:
let
  telegram-notify-env = "/home/${custom.username}/.nixos/secrets/passwords/telegram_notify_env";

  send-to-telegram = pkgs.writeShellScript "send-to-telegram" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${telegram-notify-env} | ${pkgs.findutils}/bin/xargs)
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
  systemd.services."unit-status-telegram@" = {
    description = "Unit Status Telegram Service";
    unitConfig = {
      After = "network.target";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = "${unit-status-telegram} %I";
    };
  };
}

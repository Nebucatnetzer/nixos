{ custom, inputs, pkgs, ... }:
let
  telegram-notify-env = "/home/${custom.username}/.nixos/secrets/passwords/telegram_notify_env";
  send-to-telegram = pkgs.writeShellScriptBin "send-to-telegram" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${telegram-notify-env} | ${pkgs.findutils}/bin/xargs)
    URL="https://api.telegram.org/bot$TELEGRAM_KEY/sendMessage"
    ${pkgs.curl}/bin/curl -s -d "chat_id=$CHAT_ID&disable_web_page_preview=1&text=$1" $URL > /dev/null
  '';
  unit-status-telegram = pkgs.writeShellScriptBin "unit-status-telegram" ''
    UNIT="$1"

    UNITSTATUS="$(systemctl status $UNIT)"
    ALERT="$(echo -e "\u26A0")"

    /run/current-system/sw/bin/send-to-telegram "$ALERT Unit failed $UNIT $ALERT
    Status:
    $UNITSTATUS"'';
in
{
  environment.systemPackages = with pkgs;
    [
      send-to-telegram
      unit-status-telegram
    ];
  systemd.services."unit-status-telegram@" = {
    description = "Unit Status Telegram Service";
    unitConfig = {
      After = "network.target";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = "/run/current-system/sw/bin/unit-status-telegram %I";
    };
  };
}

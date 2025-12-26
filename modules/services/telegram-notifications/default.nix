{
  config,
  inputs,
  pkgs,
  ...
}:
let
  send-to-telegram = pkgs.writeShellScriptBin "send-to-telegram" ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${config.age.secrets.telegramNotifyEnv.path}
    URL="https://api.telegram.org/bot$TELEGRAM_KEY/sendMessage"
    ${pkgs.curl}/bin/curl -s -d "chat_id=$CHAT_ID&disable_web_page_preview=1&text=$1" $URL > /dev/null'';

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
      ExecStart = "${unit-status-telegram} %I";
    };
  };
}

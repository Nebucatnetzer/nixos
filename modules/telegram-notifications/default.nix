{ custom, inputs, pkgs, ... }:
let
  telegram-notify-env = "/home/${custom.username}/.nixos/secrets/passwords/telegram_notify_env";
  telegram = pkgs.writeShellScriptBin "telegram" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${telegram-notify-env} | ${pkgs.findutils}/bin/xargs)
    URL="https://api.telegram.org/bot$TELEGRAM_KEY/sendMessage"
    curl -s -d "chat_id=$CHAT_ID&disable_web_page_preview=1&text=$1" $URL > /dev/null
  '';
in
{
  environment.systemPackages = with pkgs;
    [
      telegram
    ];
}

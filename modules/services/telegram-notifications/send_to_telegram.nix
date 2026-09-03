{
  curl,
  envFile,
  writeShellScriptBin,
}:
writeShellScriptBin "send-to-telegram" ''
  while IFS='=' read -r key value; do
      # Skip lines starting with # or empty lines
      if [[ ! $key =~ ^# && -n $key ]]; then
          export "$key=$value"
      fi
  done <${envFile}
  URL="https://api.telegram.org/bot$TELEGRAM_KEY/sendMessage"
  ${curl}/bin/curl -s -d "chat_id=$CHAT_ID&disable_web_page_preview=1&text=$1" $URL > /dev/null''

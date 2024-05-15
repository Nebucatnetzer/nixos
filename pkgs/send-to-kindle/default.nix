{
  bash,
  cacert,
  curl,
  gnome,
  gnused,
  perl,
  recode,
  writeShellApplication,
}:
writeShellApplication {
  name = "send-to-kindle";
  runtimeInputs = [
    bash
    cacert
    curl
    gnused
    gnome.nautilus
    perl
    recode
  ];
  excludeShellChecks = [ "SC2001" ];
  text = ''
    url=$1
    # get the title of the page
    title=$(curl -s "$url" | perl -l -0777 -ne 'print $1 if /<title.*?>\s*(.*?)\s*<\/title/si' | recode html..)
    # remove non-ascii characters
    title=$(echo "$title" | LANG=C sed 's/[^[:blank:][:print:]]//g')
    # remove special characters
    title=$(echo "$title" | sed 's/[^a-zA-Z0-9]/_/g')
    # replace multiple underlines with a single one
    title=$(echo "$title" | sed 's/_\+/_/g')

    curl \
      --data-urlencode "param=" \
      --data-urlencode "context=download" \
      --data-urlencode "format=epub" \
      --data-urlencode "url=$url" \
      --location https://pushtokindle.fivefilters.org/send2-html.php \
      --output "$HOME/Downloads/$title.epub"

    nautilus "$HOME"/Downloads
  '';
}

{ inputs, config, ... }:
{
  # The ddclient prestart runs as root and copies the file, so the default 0400 is enough.
  age.secrets.ddclientPassword.file = "${inputs.self}/scrts/ddclient_password.txt.age";
  services.ddclient = {
    domains = [
      "pfannedechu.ch"
      "zweili.ch"
      "www.zweili.ch"
      "zweili.org"
      "actual-andreas.zweili.org"
      "actual-benjamin.zweili.org"
      "actual-eleonore.zweili.org"
      "git.zweili.org"
      "rss.zweili.org"
      "search.zweili.org"
      "searxng.zweili.org"
    ];
    enable = true;
    passwordFile = config.age.secrets.ddclientPassword.path;
    protocol = "dyndns2";
    server = "infomaniak.com";
    ssl = true;
    use = "web";
    usev4 = "";
    usev6 = "";
    username = "7SnfujMg4GF9SKQ9";
  };
}

{ inputs, config, ... }:
{
  age.secrets.ddclientPassword = {
    file = "${inputs.self}/scrts/ddclient_password.txt.age";
    mode = "444";
  };
  services.ddclient = {
    domains = [
      "pfannedechu.ch"
      "zweili.ch"
      "www.zweili.ch"
      "zweili.org"
      "actual.zweili.org"
      "dav.zweili.org"
      "eactual.zweili.org"
      "git.zweili.org"
      "rss.zweili.org"
      "rss-bridge.zweili.org"
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

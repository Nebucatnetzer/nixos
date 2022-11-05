{ custom, hostname }: { ... }:
let
  domain = "test.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/logs-share"
    (import "${custom.inputs.self}/modules/restic-server-client" {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
      inherit custom;
    })
    "${custom.inputs.self}/modules/tmux"
  ];
  services.nginx.virtualHosts."${domain}".locations = {
    "/".extraConfig = ''
      try_files $uri $uri/ = 404;
    '';
    "/tt-rss/cache".extraConfig = ''
      aio threads;
      internal;
    '';
    "/tt-rss/backups".extraConfig = ''
      internal;
    '';
  };
}

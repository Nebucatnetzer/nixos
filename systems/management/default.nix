{ custom, hostname, inputs }: { ... }:
let
  domain = "test.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    "${inputs.self}/modules/logs-share"
    (import "${inputs.self}/modules/restic-server-client" {
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
      inherit inputs;
    })
    "${inputs.self}/modules/tmux"
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

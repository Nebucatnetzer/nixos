{ hostname, inputs, pkgs, ... }:
let
  domain = "test.2li.ch";
in
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.150";
      inherit hostname inputs pkgs;
    })
    (import "${inputs.self}/modules/nginx-fpm" {
      dataDir = "/var/www/ttrss/app";
      inherit domain inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    # "${inputs.self}/modules/logs-share"
    # I currently can't install lnav because it is not building on aarch64
    # https://github.com/NixOS/nixpkgs/issues/197512
    "${inputs.self}/modules/nix-direnv"
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

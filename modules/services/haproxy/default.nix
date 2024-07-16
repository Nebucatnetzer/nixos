{ config, lib, ... }:
let
  cfg = config.services.az-haproxy;
in
{
  options = {
    services.az-haproxy.enable = lib.mkEnableOption "Enable HAProxy";
  };

  config = lib.mkIf cfg.enable {
    networking = {
      enableIPv6 = false;
      firewall.allowedTCPPorts = [
        80
        443
        1936
      ];
    };
    services.haproxy = {
      enable = true;
      config = ''
        defaults
          log stdout  format raw  local0  info
          option tcplog
          timeout connect 5s
          timeout client  30s
          timeout server  30s

        listen haproxy-monitoring
          bind *:1936
          mode http
          stats enable
          stats hide-version
          stats realm Haproxy\ Statistics
          stats uri /
          stats auth admin:password

        frontend http
          bind *:80
          mode http
          redirect scheme https code 301 if { hdr(host) -i mail.zweili.org ! {ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i git.2li.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i heimdall.2li.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i nextcloud.2li.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i firefly.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i rss.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i rss-bridge.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i cache.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i www.2li.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr_dom(host) -i 2li.ch } !{ ssl_fc }

        frontend https
          # Listen on port 443
          bind *:443
          mode tcp

          tcp-request inspect-delay 5s
          tcp-request content accept if { req_ssl_hello_type 1 }

          # Figure out which backend (= VM) to use
          use_backend git_server if { req_ssl_sni -i git.2li.ch }
          use_backend proxy if { req_ssl_sni -i heimdall.2li.ch }
          use_backend mail_server if { req_ssl_sni -i mail.zweili.org }
          use_backend nextcloud_server if { req_ssl_sni -i nextcloud.2li.ch }
          use_backend budget_server if { req_ssl_sni -i firefly.zweili.org }
          use_backend rss_server if { req_ssl_sni -i rss.zweili.org }
          use_backend rss_server if { req_ssl_sni -i rss-bridge.zweili.org }
          use_backend cache_server if { req_ssl_sni -i cache.zweili.org }
          use_backend proxy if { req_ssl_sni -i www.2li.ch }
          use_backend proxy if { req_ssl_sni -i 2li.ch }

        backend grav_server
          mode tcp
          server server1 10.7.89.102:443 check
        backend git_server
          mode tcp
          server server1 10.7.89.109:443 check
        backend nextcloud_server
          mode tcp
          server server1 10.7.89.103:443 check
        backend budget_server
          mode tcp
          server server1 10.7.89.113:443 check
        backend rss_server
          mode tcp
          server server1 10.7.89.115:443 check
        backend mail_server
          mode tcp
          server server1 10.7.89.123:443 check
        backend proxy
          mode tcp
          server server1 127.0.0.1:4433 check
        backend cache_server
          mode tcp
          timeout client  300s
          timeout server  300s
          server server1 10.7.89.150:443 check
      '';
    };
  };
}

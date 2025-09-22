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

    services.snmpd.configText = ''
      # monitor haproxy
      proc haproxy
    '';
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
          redirect scheme https code 301 if { hdr(host) -i git.2li.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i search.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i actual.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i eactual.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i rss.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i librenms.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i rss-bridge.zweili.org } !{ ssl_fc }
          redirect scheme https code 301 if { hdr(host) -i www.zweili.ch } !{ ssl_fc }
          redirect scheme https code 301 if { hdr_dom(host) -i zweili.ch } !{ ssl_fc }

        frontend https
          # Listen on port 443
          bind *:443
          mode tcp

          tcp-request inspect-delay 5s
          tcp-request content accept if { req_ssl_hello_type 1 }

          # Figure out which backend (= VM) to use
          use_backend git_server if { req_ssl_sni -i git.2li.ch }
          use_backend proxy if { req_ssl_sni -i search.zweili.org }
          use_backend budget_server if { req_ssl_sni -i actual.zweili.org }
          use_backend budget_server if { req_ssl_sni -i eactual.zweili.org }
          use_backend rss_server if { req_ssl_sni -i rss.zweili.org }
          use_backend rss_server if { req_ssl_sni -i rss-bridge.zweili.org }
          use_backend librenms if { req_ssl_sni -i librenms.zweili.org }
          use_backend proxy if { req_ssl_sni -i www.zweili.ch }
          use_backend proxy if { req_ssl_sni -i zweili.ch }

        backend git_server
          mode tcp
          server server1 10.7.89.109:443 check
        backend budget_server
          mode tcp
          server server1 10.7.89.113:443 check
        backend rss_server
          mode tcp
          server server1 10.7.89.115:443 check
        backend librenms
          mode tcp
          server server1 10.7.89.153:443 check
        backend proxy
          mode tcp
          server server1 127.0.0.1:4433 check
      '';
    };
  };
}

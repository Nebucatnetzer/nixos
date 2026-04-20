{ config, ... }:
{
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
        bind ${config.az-hosts.gwyn.wgIp}:1936
        mode http
        stats enable
        stats hide-version
        stats realm Haproxy\ Statistics
        stats uri /
        stats auth admin:password

      frontend http
        bind *:80
        mode http
        redirect scheme https code 301 if { hdr(host) -i actual.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i dav.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i eactual.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i git.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i rss-bridge.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i rss.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i search.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i searxng.zweili.org } !{ ssl_fc }
        redirect scheme https code 301 if { hdr(host) -i www.zweili.ch } !{ ssl_fc }
        redirect scheme https code 301 if { hdr_dom(host) -i zweili.ch } !{ ssl_fc }

        use_backend gwynHTTP if { hdr_dom(host) -i vpn.zweili.org }

      frontend https
        # Listen on port 443
        bind *:443
        mode tcp

        tcp-request inspect-delay 5s
        tcp-request content accept if { req_ssl_hello_type 1 }

        # Figure out which backend to use
        use_backend gwynHTTPS if { req_ssl_sni -i actual.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i dav.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i eactual.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i git.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i search.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i searxng.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i rss-bridge.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i rss.zweili.org }
        use_backend gwynHTTPS if { req_ssl_sni -i www.zweili.ch }
        use_backend gwynHTTPS if { req_ssl_sni -i zweili.ch }

      backend gwynHTTP
        mode http
        server nginxHTTP 127.0.0.1:8080 check
      backend gwynHTTPS
        mode tcp
        server nginxHTTPS 127.0.0.1:8443 check
    '';
  };
}

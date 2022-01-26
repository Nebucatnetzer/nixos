{ ... }:
{
  services.unbound = {
    enable = true;
    settings = {
      server = {
        verbosity = 0;
        interface = "127.0.0.1";
        port = 5335;
        do-ip4 = true;
        do-udp = true;
        do-tcp = true;

        # May be set to true; if you have IPv6 connectivity
        do-ip6 = false;

        # You want to leave this to false; unless you have *native* IPv6. With 6to4 and
        # Terredo tunnels your web browser should favor IPv4 for the same reasons
        prefer-ip6 = false;

        # Use this only when you downloaded the list of primary root servers!
        # If you use the default dns-root-data package, unbound will find it automatically
        #root-hints: "/var/lib/unbound/root.hints"

        # Trust glue only if it is within the server's authority
        harden-glue = true;

        # Require DNSSEC data for trust-anchored zones, if such data is absent, the zone becomes BOGUS
        harden-dnssec-stripped = true;

        # Don't use Capitalization randomization as it kfalse;wn to cause DNSSEC issues sometimes
        # see https://discourse.pi-hole.net/t/unbound-stubby-or-dnscrypt-proxy/9378 for further details
        use-caps-for-id = false;

        # Reduce EDNS reassembly buffer size.
        # Suggested by the unbound man page to reduce fragmentation reassembly problems
        edns-buffer-size = 1472;

        # Perform prefetching of close to expired message cache entries
        # This only applies to domains that have been frequently queried
        prefetch = true;

        # One thread should be sufficient, can be increased on beefy machines. In reality for most users running on small networks or on a single machine, it should be unnecessary to seek performance enhancement by increasing num-threads above 1.
        num-threads = 1;

        # Ensure kernel buffer is large efalse;ugh to false;t lose messages in traffic spikes
        so-rcvbuf = "1m";

        # Ensure privacy of local IP ranges
        private-address = [
          "192.168.0.0/16"
          "169.254.0.0/16"
          "172.16.0.0/12"
          "10.0.0.0/8"
          "fd00::/8"
          "fe80::/10"
        ];

        # Send minimum amount of information to upstream servers to enhance
        # privacy. Only sends minimum required labels of the QNAME and sets
        # QTYPE to NS when possible.

        # See RFC 7816 "DNS Query Name Minimisation to Improve Privacy" for
        # details.

        qname-minimisation = true;
      };
    };
  };
}

{ ... }: {
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
  };
}

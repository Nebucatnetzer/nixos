{ ... }: {
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
  };
  networking.interfaces.enp1s0 = {
    useDHCP = true;
  };
}

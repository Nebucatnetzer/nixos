{ pkgs, ... }:
{
  networking.nat = {
    enable = true;
    externalInterface = "enp58s0u1";
    internalInterfaces = [ "wg0" ];
  };
  networking.wg-quick.interfaces = {
    wg0 = {
      postUp = ''
        ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.70.89.1/24 -o enp58s0u1 -j MASQUERADE
      '';

      preDown = ''
        ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.70.89.1/24 -o enp58s0u1 -j MASQUERADE
      '';
    };
  };
}

# WireGuard hub (gwyn): peers with every spoke by its /32 and forwards traffic
# between them and out to the LAN. Spokes point their single peer at this host
# (see client.nix). Add a spoke by appending one line to `spokes` below.
{
  IP,
  privateKeyFile,
}:
{
  config,
  pkgs,
  ...
}:
let
  port = 51820;
  # interface the hub masquerades spoke traffic out of (LAN uplink)
  externalInterface = "enp58s0u1";
  spokes = [
    {
      publicKey = "BzZMupEJwJXzZMHDRXcLLLPsHI8HLORiy44vbekvBzU="; # capricorn
      ip = config.az-hosts.capricorn.wgIp;
    }
    {
      publicKey = "bXKQTnyw7G93I0xktIa7kitL5kK3cMhSBAvuOOe0V3w="; # fenoglio
      ip = config.az-hosts.fenoglio.wgIp;
    }
    {
      publicKey = "ZbaHLibDBi0wFMERul8lVL3MovagB6seTS9UoVtiKDc="; # phone
      ip = config.az-hosts.phone.wgIp;
    }
    {
      publicKey = "MGmlmY7BnJrFLOyF8CzXAL1p7/mOe9Y2EYqGyRGHSSk="; # (unnamed peer)
      ip = "10.70.89.180";
    }
  ];
in
{
  networking.firewall.allowedUDPPorts = [ port ];

  # forward + masquerade so spokes reach the LAN/internet and each other (relay)
  networking.nat = {
    enable = true;
    inherit externalInterface;
    internalInterfaces = [ "wg0" ];
  };

  networking.wg-quick.interfaces.wg0 = {
    inherit privateKeyFile;
    address = [ "${IP}/32" ];
    listenPort = port;

    postUp = ''
      ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
      ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.70.89.0/24 -o ${externalInterface} -j MASQUERADE
    '';
    preDown = ''
      ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
      ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.70.89.0/24 -o ${externalInterface} -j MASQUERADE
    '';

    peers = map (spoke: {
      inherit (spoke) publicKey;
      allowedIPs = [ "${spoke.ip}/32" ];
    }) spokes;
  };
}

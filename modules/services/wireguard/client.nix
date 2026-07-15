# WireGuard spoke: a single peer (the hub, gwyn). All overlay traffic — and, for
# roaming spokes, the whole LAN — is sent through the hub, which relays
# spoke-to-spoke. See hub.nix for the other end.
{
  IP,
  privateKeyFile,
  # false for a spoke that already sits on the hub's LAN, so 10.7.89.0/24 isn't
  # routed through wg0 (the connected route would collide in wg-quick)
  routeLan ? true,
  # public host by default; a spoke on the hub's LAN passes gwyn's LAN IP so the
  # encrypted traffic goes straight over the LAN instead of hairpinning via NAT
  hubHost ? "zweili.org",
}:
{
  lib,
  ...
}:
let
  port = 51820;
in
{
  networking.firewall.allowedUDPPorts = [ port ];

  networking.wg-quick.interfaces.wg0 = {
    inherit privateKeyFile;
    address = [ "${IP}/32" ];
    listenPort = port;

    peers = [
      {
        # gwyn, acting as hub
        publicKey = "A96R1JeeZqT+pz15Dfl++1zmW8hpyefScQDNcxwB5l4=";
        allowedIPs = [
          "10.70.89.0/24"
        ]
        ++ lib.optional routeLan "10.7.89.0/24";
        endpoint = "${hubHost}:${toString port}";
        # keep the hub's knowledge of this spoke's endpoint fresh (and hold any NAT
        # hairpin mapping open for spokes reaching the hub via the public IP)
        persistentKeepalive = 25;
      }
    ];
  };
}

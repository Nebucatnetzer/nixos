# WireGuard hub (gwyn): peers with every spoke by its /32 and relays traffic between
# the spokes. Only spokes with forwardLan reach the LAN through the hub; the others
# reach gwyn itself and the other spokes. Spokes point their single peer at this host
# (see client.nix). Add a spoke by appending an entry to `spokes` below.
{
  IP,
  privateKeyFile,
}:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  port = 51820;
  # interface the hub masquerades spoke traffic out of (LAN uplink)
  externalInterface = "enp58s0u1";
  iptables = "${pkgs.iptables}/bin/iptables";
  # action is --append for postUp and --delete for preDown, so both lists stay identical.
  forwardRules =
    action:
    lib.concatMapStrings (rule: "${iptables} ${action} FORWARD ${rule}\n") (
      [
        "--in-interface wg0 --match conntrack --ctstate ESTABLISHED,RELATED --jump ACCEPT"
        "--in-interface wg0 --out-interface wg0 --jump ACCEPT"
      ]
      ++ map (spoke: "--in-interface wg0 --source ${spoke.ip} --jump ACCEPT") (
        lib.filter (spoke: spoke.forwardLan) spokes
      )
      ++ [ "--in-interface wg0 --jump DROP" ]
    );
  spokes = [
    {
      publicKey = "BzZMupEJwJXzZMHDRXcLLLPsHI8HLORiy44vbekvBzU="; # capricorn
      ip = config.az-hosts.capricorn.wgIp;
      forwardLan = true;
    }
    {
      publicKey = "bXKQTnyw7G93I0xktIa7kitL5kK3cMhSBAvuOOe0V3w="; # fenoglio
      ip = config.az-hosts.fenoglio.wgIp;
      forwardLan = true;
    }
    {
      publicKey = "nJaY/NeJSUvpZJB/9s3CHMIIJvgdU7MbzfrpRDZlTEs="; # groeibschi
      ip = config.az-hosts.groeibschi.wgIp;
      forwardLan = false;
    }
    {
      publicKey = "B10bbtGdcU+GTP3bCquucl8J9tzjjgywlAWFS/S/FzE="; # flunder
      ip = config.az-hosts.flunder.wgIp;
      forwardLan = false;
    }
    {
      publicKey = "MGmlmY7BnJrFLOyF8CzXAL1p7/mOe9Y2EYqGyRGHSSk="; # (unnamed peer)
      ip = "10.70.89.180";
      forwardLan = false;
    }
  ];
in
{
  networking.firewall.allowedUDPPorts = [ port ];

  # masquerade so forwardLan spokes reach the LAN; forwardRules decides who is forwarded
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
      ${forwardRules "--append"}
      ${iptables} -t nat -A POSTROUTING -s 10.70.89.0/24 -o ${externalInterface} -j MASQUERADE
    '';
    preDown = ''
      ${forwardRules "--delete"}
      ${iptables} -t nat -D POSTROUTING -s 10.70.89.0/24 -o ${externalInterface} -j MASQUERADE
    '';

    peers = map (spoke: {
      inherit (spoke) publicKey;
      allowedIPs = [ "${spoke.ip}/32" ];
    }) spokes;
  };
}

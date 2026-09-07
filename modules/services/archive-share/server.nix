{
  # One export spec per allowed client. Explicit rather than the whole wg subnet: the hub
  # also peers with a phone and a spare peer that have no business mounting this.
  clients,
  path,
}:
{ lib, ... }:
{
  services.nfs.server = {
    enable = true;
    exports = ''
      ${path} ${lib.concatMapStringsSep " " (client: "${client}(rw,sync,no_subtree_check)") clients}
    '';
  };

  # v4 only, so 2049 only.
  services.nfs.settings.nfsd.vers3 = false;
  # Limited to Wireguard
  networking.firewall.interfaces.wg0.allowedTCPPorts = [ 2049 ];

  # Never serve an empty directory. The archive mount carries nofail, so without this a
  # missing disk would export the bare mountpoint on the root filesystem and a client
  # would see an archive that had lost everything. Requires also stops the server when
  # the mount goes away, instead of leaving it exporting the wrong tree.
  systemd.services.nfs-server.unitConfig.RequiresMountsFor = path;
}

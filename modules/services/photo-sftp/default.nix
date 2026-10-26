# SFTP access for PhotoSync on the iPhone (groeibschi): read the photo library, upload new photos.
# A separate sshd serves only this user, so the main sshd stays unchanged.
{
  config,
  pkgs,
  ...
}:
let
  user = "photosync";
  port = 2222;
  chrootDirectory = "/srv/${user}";
  photoDirectory = "/home/${config.az-username}/data/20_pictures/photos";
  uploadDirectory = "/home/${config.az-username}/Downloads/inbox/photos/iphone";
  phoneIp = config.az-hosts.groeibschi.wgIp;

  authorizedKeysFile = "ssh/${user}_authorized_keys";

  sshdConfig = pkgs.writeText "${user}-sshd-config" ''
    Port ${toString port}
    HostKey /etc/ssh/ssh_host_ed25519_key
    PidFile none
    # NixOS locks system users with "!". Without PAM, sshd rejects locked accounts.
    UsePAM yes
    # sshd rejects key files below /nix/store, because the store is group-writable.
    AuthorizedKeysFile /etc/${authorizedKeysFile}
    AllowUsers ${user}@${phoneIp}
    AuthenticationMethods publickey
    PasswordAuthentication no
    KbdInteractiveAuthentication no
    PermitRootLogin no
    DisableForwarding yes
    PermitTTY no
    ChrootDirectory ${chrootDirectory}
    # No -R, because uploads need writes. The photos mount is read-only by itself.
    ForceCommand internal-sftp -d /photos
    Subsystem sftp internal-sftp
  '';
in
{
  users.groups.${user} = { };
  users.users.${user} = {
    isSystemUser = true;
    group = user;
  };

  # A mode makes NixOS copy the file, not link it to the store.
  environment.etc.${authorizedKeysFile} = {
    mode = "0444";
    text = ''
      from="${phoneIp}",restrict ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2YvQt6LGe4b79bYxpjZ7dwdUYjiDP+BUU4CvF5nLKe photosync@groeibschi
    '';
  };

  # sshd refuses a chroot unless root owns every directory in its path.
  systemd.tmpfiles.rules = [
    "d ${chrootDirectory} 0755 root root -"
  ];

  # bindfs shows the photos as owned by the photosync user and blocks all writes.
  fileSystems."${chrootDirectory}/photos" = {
    device = photoDirectory;
    fsType = "fuse.bindfs";
    noCheck = true;
    options = [
      "ro"
      "force-user=${user}"
      "force-group=${user}"
      "perms=0000:u+rD"
      "x-systemd.requires-mounts-for=${photoDirectory}"
    ];
  };

  # Uploaded files belong to the main user on disk. The phone cannot delete anything here.
  fileSystems."${chrootDirectory}/upload" = {
    device = uploadDirectory;
    fsType = "fuse.bindfs";
    noCheck = true;
    options = [
      "force-user=${user}"
      "force-group=${user}"
      "perms=0000:u+rwD"
      "create-for-user=${config.az-username}"
      "create-for-group=${config.az-username}"
      "create-with-perms=0644:a+D"
      "chown-ignore"
      "chmod-ignore"
      "delete-deny"
      "nofail"
      "x-systemd.requires-mounts-for=${uploadDirectory}"
    ];
  };

  systemd.services.photosync-sshd = {
    description = "SSH daemon for PhotoSync access";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network.target"
      "sshd.service" # sshd creates the host keys on first boot.
    ];
    serviceConfig = {
      # sshd needs an absolute path to re-execute itself.
      ExecStart = "${config.services.openssh.package}/bin/sshd -D -e -f ${sshdConfig}";
      Restart = "on-failure";
    };
  };

  # Only the phone may reach this sshd, and only through WireGuard.
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -i wg0 -p tcp --dport ${toString port} -s ${phoneIp} -j nixos-fw-accept
  '';
}

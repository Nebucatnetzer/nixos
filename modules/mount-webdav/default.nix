{ custom }: { ... }: {
  age.secrets.webdavSecrets = {
    file = "${custom.inputs.self}/scrts/webdav_andreas.age";
    path = "/home/${custom.username}/.davfs2/secrets";
    mode = "600";
    owner = custom.username;
    group = "users";
  };
  services.davfs2 = {
    enable = true;
    davUser = custom.username;
    # extraConfig = ''
    #   use_locks 0
    # '';
  };
  fileSystems."/home/${custom.username}/10_documents" = {
    device = "https://nextcloud.2li.ch/remote.php/dav/files/${custom.username}";
    fsType = "davfs";
    options = [
      "user"
      "rw"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=300"
      "noatime"
    ];
  };
}

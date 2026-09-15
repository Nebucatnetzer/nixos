# Cold offsite tier: a restic repository on a USB disk that is attached a few times a
# year. Everything is noauto, because the disk is absent most of the time, and the
# refresh is triggered by udev rather than by a timer.
{
  # LUKS UUID of the partition holding the repository. Device names change between
  # replugs, the UUID does not.
  luksUuid,
  # Where gwyn's archive is automounted on this host.
  archivePath ? "/mnt/archive-disk",
  mountPath ? "/mnt/cold-offsite",
}:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  hddBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/hdd_options.nix";
  luksName = "coldOffsiteLuks";
  passwordFile = config.age.secrets.resticKey.path;
  repository = "${mountPath}/restic-repo";
  # The unit writes the repository as root, so an interactive command has to be root too.
  # Two uids writing one repository is the mistake that broke every client on gwyn on
  # 2026-09-07. sudo comes from PATH on purpose: the setuid binary is in /run/wrappers,
  # the one in the store is not setuid and refuses to run.
  restic-cold = pkgs.writeShellApplication {
    name = "restic-cold";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      exec sudo env RESTIC_CACHE_DIR=/var/cache/restic \
        ${pkgs.restic}/bin/restic \
          --repo ${repository} \
          --password-file ${passwordFile} \
          "$@"
    '';
  };
  sharedExcludeFile = "${inputs.self}/modules/misc/restic/client_excludes.txt";
  sourceRepository = "rest:http://${config.az-hosts.gwyn.wgIp}:8123";
  telegramNotifications = "${inputs.self}/modules/services/telegram-notifications";
  unitName = "restic-cold-refresh";
in
{
  imports = [
    telegramNotifications
  ];

  age.secrets.coldOffsiteLuksKey = {
    file = "${inputs.self}/scrts/${config.networking.hostName}_cold_offsite_luks.key.age";
    mode = "400";
  };
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

  # Stage 2 crypttab, the same route gwyn's archive disk uses: nixpkgs has no option for a
  # non-root LUKS device. key-slot=1 pins the unlock to the keyfile slot, so cryptsetup
  # never tries the interactive slot 0 first and pays its KDF run before failing there.
  environment.etc.crypttab.text = ''
    ${luksName} UUID=${luksUuid} ${config.age.secrets.coldOffsiteLuksKey.path} luks,noauto,key-slot=1,x-systemd.device-timeout=60
  '';

  environment.systemPackages = [
    restic-cold
  ];

  # noauto keeps boot away from a disk that is usually unplugged, and it also stops udisks
  # from claiming the disk into /run/media. A noauto crypttab entry leaves
  # systemd-cryptsetup@ wanted by nothing, so the mount has to require that unit by name.
  # A plain device dependency would wait for a device nothing ever creates.
  fileSystems."${mountPath}" = {
    device = "/dev/mapper/${luksName}";
    fsType = "btrfs";
    neededForBoot = false;
    # Appended after the hdd options, not before: those contain "defaults", which implies
    # auto, and systemd takes the last occurrence of the option.
    options = hddBtrfsOptions ++ [
      "noauto"
      "nofail"
      "x-systemd.device-timeout=60"
      "x-systemd.requires=systemd-cryptsetup@${luksName}.service"
    ];
  };

  # A disk that is usually unplugged is a disk a timer never catches. ID_FS_UUID on the
  # partition is the LUKS UUID, so the rule fires while the container is still locked.
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="block", ENV{ID_FS_UUID}=="${luksUuid}", ENV{SYSTEMD_WANTS}+="${unitName}.service"
  '';

  systemd.services."${unitName}" = {
    description = "Refresh the cold offsite repository on ${mountPath}";
    # The mount requires systemd-cryptsetup@ by name, so this one dependency covers the
    # unlock as well.
    unitConfig = {
      RequiresMountsFor = [ mountPath ];
    };
    serviceConfig = {
      # Shared with the hourly client job deliberately: same uid and the same source
      # repository, so the index cache is already warm.
      CacheDirectory = "restic";
      Type = "oneshot";
      User = "root";
    };
    environment = {
      RESTIC_CACHE_DIR = "/var/cache/restic";
      RESTIC_FROM_PASSWORD_FILE = passwordFile;
      RESTIC_FROM_REPOSITORY = sourceRepository;
      RESTIC_PASSWORD_FILE = passwordFile;
      RESTIC_REPOSITORY = repository;
    };
    onFailure = [ "unit-status-telegram@%N.service" ];
    script = ''
      if ! ${pkgs.netcat}/bin/nc -vzw 2 ${config.az-hosts.gwyn.wgIp} 8123 >/dev/null 2>&1; then
        echo "gwyn is unreachable. Refresh skipped."
        exit 0
      fi

      if ! ${pkgs.util-linux}/bin/mountpoint --quiet ${mountPath}; then
        echo "${mountPath} is not a mountpoint. Refresh aborted."
        exit 1
      fi

      # No auto-init: an empty or wrong disk would answer with a second empty repository
      # that nothing would notice until a restore.
      if [ ! -f ${repository}/config ]; then
        echo "No repository at ${repository}. Initialise it by hand with restic-cold."
        exit 1
      fi

      echo "Copy the system snapshots from gwyn."
      ${pkgs.restic}/bin/restic copy --retry-lock 30m

      # ls triggers the automount. mountpoint is no use for this check, because the autofs
      # is a mountpoint even when the NFS behind it never mounts.
      ${pkgs.coreutils}/bin/ls ${archivePath} >/dev/null 2>&1 || true
      if ! ${pkgs.util-linux}/bin/findmnt --types nfs4 --mountpoint ${archivePath} >/dev/null; then
        echo "${archivePath} is not mounted over NFS. Archive backup aborted."
        exit 1
      fi

      echo "Back up the archive."
      ${pkgs.restic}/bin/restic backup \
        --exclude-file=${sharedExcludeFile} \
        --one-file-system \
        --tag archive ${archivePath}

      echo "Check the cold repository."
      ${pkgs.restic}/bin/restic check
    '';
  };
}

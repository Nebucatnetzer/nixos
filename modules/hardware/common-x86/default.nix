{
  config,
  inputs,
  pkgs,
  ...
}:
{
  age.secrets.mailPassword.file = "${inputs.self}/scrts/mail_password.age";

  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  # Enable boot splash screen
  boot.plymouth.enable = true;
  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  boot.kernelModules = [
    "kvm-intel"
    "sg"
  ];
  boot.supportedFilesystems = [
    "btrfs"
    "cifs"
    "exfat"
    "ext4"
    "f2fs"
    "nfs"
    "nfs4"
    "ntfs"
    "squashfs"
  ];
  hardware = {
    cpu.intel.updateMicrocode = true;
    graphics.enable = true;
  };
  nixpkgs.hostPlatform = "x86_64-linux";
  programs.msmtp = {
    enable = true;
    defaults = {
      port = 465;
      tls = true;
      tls_starttls = false; # <-- CRITICAL FOR PORT 465
    };
    setSendmail = true;
    accounts = {
      default = {
        auth = true;
        from = "admin@zweili.ch";
        host = "mail.infomaniak.com";
        passwordeval = "${pkgs.coreutils}/bin/cat ${config.age.secrets.mailPassword.path}";
        user = "admin@zweili.ch";
      };
    };
  };
  services = {
    fstrim.enable = true;
    hardware.bolt.enable = true;
    smartd = {
      enable = true;
      autodetect = false;
      notifications.mail = {
        enable = true;
        recipient = "admin+smartd@zweili.ch";
        sender = "admin@zweili.ch";
      };
    };
    thermald.enable = true;
  };
}

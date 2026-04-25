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
  hardware = {
    cpu.intel.updateMicrocode = true;
  };
  nixpkgs.hostPlatform = "x86_64-linux";
  services.smartd = {
    enable = true;
    autodetect = true;
    notifications.mail = {
      enable = true;
      recipient = "admin+smartd@zweili.ch";
      sender = "admin@zweili.ch";
    };
  };
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
}

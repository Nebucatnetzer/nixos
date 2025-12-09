{ ... }:
{
  fileSystems."/var/log" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "defaults"
      "size=300M"
    ];
  };
  services.journald.extraConfig = ''
    SystemMaxUse=300M
    Storage=volatile
  '';
}

{ pkgs, ... }:
{
  fileSystems."/var/log" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=512M" ];
  };
  services.journald.extraConfig = ''
    SystemMaxUse=300M
    Storage=volatile
  '';
}

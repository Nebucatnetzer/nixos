{ pkgs, ... }:
{
  fileSystems."/tmp/log " = {
    fsType = "tmpfs";
    options = [ "size=512M" ];
  };
  services.journald.extraConfig = ''
    SystemMaxUse=300M
  '';
}

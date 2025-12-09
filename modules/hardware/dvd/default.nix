{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.libaacs
    pkgs.libbluray
    pkgs.libdvdcss
    pkgs.libdvdnav
    pkgs.libdvdread
  ];
}

{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libaacs
    libbluray
    libdvdcss
    libdvdnav
    libdvdread
  ];
}

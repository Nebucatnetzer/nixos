{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libbluray
    libdvdcss
    libdvdnav
    libdvdread
  ];
}

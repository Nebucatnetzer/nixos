{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-distrobox;
  boosteroid = pkgs.writeShellScriptBin "boosteroid" ''
    ${pkgs.unstable.distrobox}/bin/distrobox enter boosteroid -- /opt/BoosteroidGamesS.R.L./bin/Boosteroid -vaapi
  '';
  boosteroid-install = pkgs.writeShellScriptBin "boosteroid-install" ''
    # setup audio
    sudo apt install -y pipewire-audio-client-libraries wireplumber &&
    sudo cp /usr/share/doc/pipewire/examples/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d/ &&
    sudo ldconfig &&
    systemctl --user --now enable wireplumber.service &&

    # install boosteroid
    sudo apt install -y libxi6 libxcb-icccm4 libxcb-image0 libxcb-keysyms1 libxcb-render-util0 libxcb-shape0 libxcb-xkb1 libxkbcommon-x11-0 libwayland-cursor0 fontconfig-config fonts-dejavu-core i965-va-driver intel-media-va-driver libc-dev-bin libc-devtools libc6-dev libcrypt-dev libdeflate0 libfontconfig1 libfreetype6 libgd3 libigdgmm12 libjbig0 libjpeg-turbo8 libjpeg8 libnsl-dev libpcre2-16-0 libpcre2-32-0 libpcre2-dev libpcre2-posix3 libpng16-16 libtiff5 libtirpc-dev libva-drm2 libva-x11-2 libva2 libvdpau1 libwebp7 libxcb-xinerama0 libxcb-xinput0 libxpm4 linux-libc-dev manpages manpages-dev mesa-va-drivers mesa-vdpau-drivers rpcsvc-proto traceroute ucf va-driver-all vdpau-driver-all &&
    wget -q --no-hsts https://boosteroid.com/linux/installer/boosteroid-install-x64.deb && sudo dpkg -i ./boosteroid-install-x64.deb && rm ./boosteroid-install-x64.deb
  '';
in
{
  options = {
    programs.az-distrobox.enable = lib.mkEnableOption "Install distrobox and configure it to run GUI applications.";
  };
  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [
        boosteroid
        boosteroid-install
        pkgs.unstable.distrobox
        pkgs.xorg.xhost
      ];
      shellInit = ''
        [ -n "$DISPLAY" ] && xhost +si:localuser:$USER || true
      '';
    };

    home-manager.users.${config.az-username} = {
      home.file.".config/distrobox/distrobox.conf".source = ./distrobox.conf;
    };
  };
}

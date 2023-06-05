{ ... }:
{
  imports = [
    ./common
    ./hardware/bluetooth
    ./hardware/dvd
    ./hardware/nvidia
    ./profiles/desktop
    ./programs/email
    ./programs/eog
    ./programs/hunspell
    ./programs/libimobiledevice
    ./programs/lockscreen
    ./programs/makemkv
    ./programs/nix-direnv
    ./programs/scripts
    ./programs/steam
    ./programs/tmux
    ./services/common-x86
    ./services/data-share
    ./services/docker
    ./services/docker-mailserver
    ./services/gitea
    ./services/grav
    ./services/haproxy
    ./heimdall
    ./services/logs-share
    ./services/log-to-ram
    ./services/pipewire
    ./services/rclone-webdav
    ./services/rdp
    ./services/restic-client-desktop
    ./services/syslog
    ./services/telegram-notifications
    ./services/tlp
    ./services/virtualbox-guest
    ./username
  ];
}

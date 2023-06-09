{ ... }:
{
  imports = [
    ./hardware/bluetooth
    ./hardware/dvd
    ./hardware/nvidia
    ./hardware/raspi4
    ./misc/common
    ./misc/username
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
    ./services/heimdall
    ./services/log-to-ram
    ./services/logs-share
    ./services/mariadb-for-containers
    ./services/media-share
    ./services/nextcloud
    ./services/nginx-acme-base
    ./services/nginx-fpm
    ./services/nginx-proxy
    ./services/pipewire
    ./services/plex
    ./services/postgresql
    ./services/qtile
    ./services/rclone-webdav
    ./services/rdp
    ./services/restic-client-desktop
    ./services/restic-client-server
    ./services/restic-client-server-mysql
    ./services/restic-client-server-postgres
    ./services/restic-server
    ./services/rss-bridge
    ./services/syslog
    ./services/telegram-notifications
    ./services/tlp
    ./services/ttrss-postgres
    ./services/virtualbox-guest
  ];
}

{ ... }: {
  imports = [
    ./hardware/bluetooth
    ./hardware/dvd
    ./hardware/nvidia
    ./hardware/raspi4
    ./misc/common
    ./misc/initrd-ssh
    ./misc/username
    ./profiles/desktop
    ./programs/distrobox
    ./programs/eog
    ./programs/libimobiledevice
    ./programs/lockscreen
    ./programs/makemkv
    ./programs/nautilus
    ./programs/nix-direnv
    ./programs/restic-management
    ./programs/scripts
    ./programs/steam
    ./services/common-x86
    ./services/data-share
    ./services/docker
    ./services/docker-mailserver
    ./services/freshrss
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
    ./services/virtualbox-guest
  ];
}

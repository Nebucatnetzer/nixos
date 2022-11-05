{ custom }: { pkgs, ... }:
{
  services.usbmuxd.enable = true;
  environment.systemPackages = with pkgs;
    [
      libimobiledevice
    ];

  home-manager.users.${custom.username} = {
    home.shellAliases = {
      iphone-backup = ''
        backup_dir=~/Downloads/$(date -I)_iphone &&
        mkdir -p $backup_dir &&
        ${pkgs.libimobiledevice}/bin/idevicebackup2 backup $backup_dir &&
        ${pkgs.gnutar}/bin/tar cjf $backup_dir.tar.bz2 $backup_dir &&
        rm -r $backup_dir'';

      ipad-backup = ''
        backup_dir=~/Downloads/$(date -I)_ipad &&
        mkdir -p $backup_dir &&
        ${pkgs.libimobiledevice}/bin/idevicebackup2 backup $backup_dir &&
        ${pkgs.gnutar}/bin/tar cjf $backup_dir.tar.bz2 $backup_dir &&
        rm -r $backup_dir'';
    };
  };
}

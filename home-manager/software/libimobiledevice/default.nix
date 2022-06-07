{ pkgs, ... }:
{
  home.packages = with pkgs; [
    libimobiledevice
  ];
  home.shellAliases = {
    iphone-backup = ''
      backup_dir=~/Downloads/$(date -I)_iphone &&
      mkdir -p $backup_dir &&
      idevicebackup2 backup $backup_dir &&
      tar cjf $backup_dir.tar.bz2 $backup_dir &&
      rm -r $backup_dir
    '';
    ipad-backup = ''
      backup_dir=~/Downloads/$(date -I)_ipad &&
      mkdir -p $backup_dir &&
      idevicebackup2 backup $backup_dir &&
      tar cjf $backup_dir.tar.bz2 $backup_dir &&
      rm -r $backup_dir
    '';
  };
}

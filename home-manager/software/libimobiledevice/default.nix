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
      zip -r $backup_dir.zip $backup_dir &&
      rm -r $backup_dir
    '';
    ipad-backup = ''
      backup_dir=~/Downloads/$(date -I)_ipad &&
      mkdir -p $backup_dir &&
      idevicebackup2 backup $backup_dir &&
      zip -r $backup_dir.zip $backup_dir &&
      rm -r $backup_dir
    '';
  };
}

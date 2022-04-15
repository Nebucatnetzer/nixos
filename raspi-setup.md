curl https://git.2li.ch/Nebucatnetzer/nixos/archive/format-sd-card.tar.gz | tar xz && cd nixos && nix-shell
sudo 'mkfs.f2fs' '-fl' 'nixos' '-O' 'extra_attr,inode_checksum,sb_checksum,compression' '/dev/mmcblk0p3'
sudo nixos-install --no-root-passwd --root /mnt --impure --flake .#raspi-test

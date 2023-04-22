{ custom }: { pkgs, ... }:
{
  home-manager.users.${custom.username} = {
    home.packages = with pkgs; [
      makemkv
    ];
  };
  boot.kernelModules = [ "sg" ];
}

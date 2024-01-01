{ inputs, pkgs, ... }: {
  imports = [ "${inputs.self}/home-manager/modules" ];
  home = {
    packages = with pkgs; [
      nodejs # needed for ansible-language-server
      plexamp
    ];
  };

  programs = {
    az-git = { userEmail = "zweili@contria.com"; };
    az-work-desktop.enable = true;
  };

  services = { az-desktop-base.enable = true; };
}


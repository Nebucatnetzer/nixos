{ inputs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];
  programs = {
    az-git = {
      userEmail = "zweili@contria.com";
    };
    az-work-desktop.enable = true;
  };

  services = {
    az-desktop-base.enable = true;
  };
}


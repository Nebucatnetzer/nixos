{ pkgs, ... }:

{
  # enable lockscreen
  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 000000";
  };

  environment.systemPackages = with pkgs; [
    i3lock
  ];
}


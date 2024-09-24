{
  inputs,
  pkgs,
  system,
  ...
}:
let
  unlock-luks = pkgs.writeShellScriptBin "unlock-luks" ''
    until ${pkgs.netcat}/bin/nc -vzw 2 $1 22; do
        sleep 1
    done &&
        ${pkgs.openssh}/bin/ssh \
          -o UserKnownHostsFile=/dev/null \
          -o StrictHostKeyChecking=no \
          -o User=root \
          $1
  '';
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    runtimeInputs = [
      pkgs.nixos-rebuild
    ];
    text = ''
      nixos-rebuild -j auto switch --use-remote-sudo
    '';
  };
in
{
  imports = [ ./headless.nix ];

  home = {
    packages = [
      pkgs.exercism
      pkgs.git
      pkgs.nix-tree
      rebuild
      unlock-luks
    ];
    shellAliases = {
      format-modules = "${pkgs.unstable.nixfmt-rfc-style}/bin/nixfmt **/*.nix";
    };
  };

  programs = {
    az-emacs.enable = true;
    az-email.enable = true;
    az-fzf.enable = true;
    az-git.enable = true;
    az-hunspell.enable = true;
    az-open-port.enable = true;
    az-ssh.enable = true;
    az-starship.enable = true;
    az-tmux.enable = true;
  };
  services.ssh-agent.enable = true;
}

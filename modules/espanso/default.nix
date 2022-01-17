{ pkgs, ... }:
let
  username = import ../../username.nix;
in
{
  services.espanso.enable = true;
  home-manager.users.${username} = {
    xdg.configFile.espanso = {
      target = "espanso/default.yml";
      onChange = "systemctl --user restart espanso";
      text = ''
        matches:
          - trigger: "<dd"
            replace: "{{current_date}}"
            vars:
              - name: current_date
                type: date
                params:
                  format: "%Y-%m-%d"
      '';
    };
  };
}


{ pkgs, ... }:
let
  username = import ../../username.nix;
in
{
  services.espanso.enable = true;
  home-manager.users.${username} = {
    xdg.configFile.espanso = {
      target = "espanso/default.yml";
      onChange = "${pkgs.espanso}/bin/espanso restart";
      text = ''
        matches:
          - trigger: "<dd"
            replace: "{{current_date}}"
            vars:
              - name: current_date
                type: date
                params:
                  format: "%YY-%m-%d"
      '';
    };
  };
}


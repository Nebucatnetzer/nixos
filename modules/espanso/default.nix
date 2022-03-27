{ inputs, custom, pkgs, ... }:
{
  services.espanso.enable = true;
  home-manager.users.${custom.username} = {
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
          - trigger: "<ds"
            replace: "{{current_date}}"
            vars:
              - name: current_date
                type: date
                params:
                  format: "%Y-%m-%d_%H%M%S"
      '';
    };
  };
}


{ config, lib, ... }:
let
  cfg = config.services.az-espanso;
in
{
  options = {
    services.az-espanso.enable = lib.mkEnableOption "Enable espanso.";
  };

  config = lib.mkIf cfg.enable {
    services.espanso = {
      enable = true;
      configs.default = {
        undo_backspace = false;
        search_shortcut = "off";
        search_trigger = "off";
      };
      matches.default.matches = [
        {
          trigger = "<dd";
          replace = "{{current_date}}";
          vars = [
            {
              name = "current_date";
              type = "date";
              params = {
                format = "%Y-%m-%d";
              };
            }
          ];
        }
        {
          trigger = "<dt";
          replace = "{{current_date}}";
          vars = [
            {
              name = "current_date";
              type = "date";
              params = {
                format = "%Y-%m-%d_%H%M%S";
              };
            }
          ];
        }
        {
          trigger = "<dn";
          replace = "{{current_date}}";
          vars = [
            {
              name = "current_date";
              type = "date";
              params = {
                format = "%Y%m%dT%H%M%S";
              };
            }
          ];
        }
      ];
    };
  };
}

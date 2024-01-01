{ config, lib, ... }:
let cfg = config.services.az-espanso;
in {
  options = {
    services.az-espanso.enable = lib.mkEnableOption "Enable espanso.";
  };

  config = lib.mkIf cfg.enable {
    services.espanso = {
      enable = true;
      configs.default = {
        undo_backspace = false;
        search_trigger = "off";
      };
      matches.default.matches = [
        {
          trigger = "<dd";
          replace = "{{current_date}}";
          vars = [{
            name = "current_date";
            type = "date";
            params = { format = "%Y-%m-%d"; };
          }];
        }
        {
          trigger = "<ds";
          replace = "{{current_date}}";
          vars = [{
            name = "current_date";
            type = "date";
            params = { format = "%Y-%m-%d_%H%M%S"; };
          }];
        }
      ];
    };
  };
}


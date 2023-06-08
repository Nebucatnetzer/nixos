{ ... }:
{
  services.espanso = {
    enable = true;
    settings = {
      undo_backspace = false;
      search_trigger = false;
      matches = [
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


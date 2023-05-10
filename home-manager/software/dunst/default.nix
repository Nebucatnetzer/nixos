{ ... }:
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Source Code Pro 11";
        markup = "yes";
        format = "<b>%s</b>\\n%b";
        sort = "no";
        indicate_hidden = "yes";
        alignment = "center";
        show_age_threshold = -1;
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = "yes";
        shrink = "no";
        transparency = 5;
        idle_threshold = 0;
        monitor = 0;
        follow = "none";
        sticky_history = "yes";
        history_length = 15;
        show_indicators = "no";
        line_height = 3;
        separator_height = 2;
        padding = 6;
        horizontal_padding = 6;
        separator_color = "frame";
        browser = "firefox - new-tab";
        icon_position = "off";
        max_icon_size = 80;
        frame_width = 3;
        frame_color = "#8EC07C";
      };
      urgency_low = {
        frame_color = "#3B7C87";
        foreground = "#3B7C87";
        background = "#191311";
        timeout = 4;
      };
      urgency_normal = {
        frame_color = "#5B8234";
        foreground = "#5B8234";
        background = "#191311";
        timeout = 6;
      };
      urgency_critical = {
        frame_color = "#B7472A";
        foreground = "#B7472A";
        background = "#191311";
        timeout = 8;
      };
    };
  };
}

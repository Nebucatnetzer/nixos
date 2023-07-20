{ config, lib, ... }:
let
  cfg = config.programs.az-tmux;
in
{
  options = {
    programs.az-tmux.enable = lib.mkEnableOption "Enable tmux";
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      extraConfig = ''
        set -g default-terminal "tmux-256color"
        set -ga terminal-overrides ",*256col*:Tc"
        set-option -g history-limit 300000

        # enable vim like key bindings
        set-window-option -g mode-keys vi
        unbind [
        bind Escape copy-mode
        unbind p
        bind p paste-buffer
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi y send-keys -X copy-selection
        bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

        # changing default delay
        set -s escape-time 1

        # window navigation with the meta key + vim keybinding
        bind -n M-h select-pane -L
        bind -n M-l select-pane -R
        bind -n M-k select-pane -U
        bind -n M-j select-pane -D

        # enable logging on key P
        bind P pipe-pane -o "cat >>~/#W.log" \; display-message "Toggled logging to ~/#W.log"

        ### Status bar design
        # status line
        set -g status-justify left
        set -g status-bg black
        set -g status-fg white
        set -g status-interval 2

        # window status
        setw -g window-status-format " #F#I:#W#F "
        setw -g window-status-current-format " #F#I:#W#F "
        setw -g window-status-format "#[fg=magenta]#[bg=black] #I#[bg=cyan]#[fg=colour8] #W "
        setw -g window-status-current-format "#[bg=brightmagenta]#[fg=colour8] #I#[fg=colour8]#[bg=colour14] #W "

        # loud or quiet?
        set-option -g visual-activity off
        set-option -g visual-bell off
        set-option -g visual-silence off
        set-window-option -g monitor-activity off
        set-option -g bell-action none

        # The modes
        setw -g clock-mode-colour colour135

        # The statusbar
        set -g status-position top
        set -g status-bg colour234
        set -g status-fg colour137
        set -g status-left ""
        set -g status-right '#[fg=colour233,bg=colour245,bold] %m-%d %H:%M:%S '
        set -g status-right-length 50
        set -g status-left-length 20
        setw -g window-status-current-format '#I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '
        setw -g window-status-format '#I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
      '';
    };
  };
}


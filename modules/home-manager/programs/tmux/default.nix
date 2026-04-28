{ ... }:
{
  programs.tmux = {
    enable = true;
    baseIndex = 1;
    escapeTime = 1;
    focusEvents = true;
    historyLimit = 300000;
    keyMode = "vi";
    terminal = "xterm-256color";
    extraConfig = ''
      set -as terminal-features ",*256col*:RGB"

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

      # loud or quiet?
      set-option -g visual-activity off
      set-option -g visual-bell off
      set-option -g visual-silence off
      set-window-option -g monitor-activity off
      set-option -g bell-action none

      # The modes
      setw -g clock-mode-colour colour135

      ### Status bar design
      set -g status-bg colour234
      set -g status-fg colour137
      set -g status-interval 2
      set -g status-justify left
      set -g status-left ""
      set -g status-left-length 20
      set -g status-position top
      set -g status-right '#[fg=colour233,bg=colour245,bold] %m-%d %H:%M:%S '
      set -g status-right-length 50
      setw -g window-status-current-format '#I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '
      setw -g window-status-format '#I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
    '';
  };
}

_: {
  programs.tmux = {
    enable = true;
    baseIndex = 1;
    # 1 ms is too tight for the WSL2 pty. Escape sequences from Windows Terminal get
    # split across reads and tmux then prints the tail as literal characters.
    escapeTime = 10;
    focusEvents = true;
    historyLimit = 300000;
    keyMode = "vi";
    terminal = "tmux-256color";
    extraConfig = ''
      set -as terminal-features ",*256col*:RGB"

      # Resize a window to the smallest client actually viewing it, not to the
      # smallest client attached to the session.
      setw -g aggressive-resize on

      # Manual escape hatch if the display still desyncs.
      bind R refresh-client \; display-message "redrawn"

      unbind [
      bind Escape copy-mode
      unbind p
      bind p paste-buffer
      bind-key -T copy-mode-vi v send-keys -X begin-selection
      bind-key -T copy-mode-vi y send-keys -X copy-selection
      bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

      # Mouse drag copies but stays in copy-mode, so scroll position survives
      # repeated selections. Leave copy-mode explicitly with q or Escape.
      bind-key -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-selection

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

{ pkgs, ... }:
{
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

      # Statusbar
      set -g status-position top
      set -g status-bg colour234
      set -g status-fg colour137
      set -g status-attr dim
    '';
  };
}

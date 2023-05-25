(use-package python-mode
  :config
  (setq python-shell-interpreter "python3"))

(use-package python-pytest
  :bind
  (:map python-mode-map ("C-c C-t" . python-pytest-function-dwim)))

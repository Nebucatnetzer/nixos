;; -*- lexical-binding: t; -*-
(use-package python-mode
  :config
  (unbind-key "C-c C-t" python-ts-mode-map)
  (setq python-shell-interpreter "python3")
  :hook (python-ts-mode . eglot-ensure))


(use-package python-pytest
  :config
  :bind
  (:map python-mode-map ("C-c C-t" . python-pytest-function-dwim)))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

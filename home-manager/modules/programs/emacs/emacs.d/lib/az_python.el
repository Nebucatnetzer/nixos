;; -*- lexical-binding: t; -*-
(use-package python-mode
  :config
  (setq python-shell-interpreter "python3")
  (setq flymake-pylint-executable "pylint")
  :hook ((python-ts-mode . eglot-ensure)
         (eglot-managed-mode . pylint-setup-flymake-backend)))


(use-package python-pytest
  :config
  (define-key python-ts-mode-map (kbd "C-c t" ) #'python-pytest-dispatch)
  )


(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (ansible . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (lsp-treemacs-sync-mode 1)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0 ;; default is 0.2
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-pylint-enabled t)
  :commands lsp)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)

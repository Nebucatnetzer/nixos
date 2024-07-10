;; -*- lexical-binding: t; -*-
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (lsp-treemacs-sync-mode 1)
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500
        lsp-keep-workspace-alive nil
        lsp-auto-register-remote-clients nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-pylint-enabled t)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\var'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.devenv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.direnv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.gitlab\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.gitlab-ci\\.yml\\'")
  :commands lsp)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
(use-package dap-mode)

;; -*- lexical-binding: t; -*-
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")

  ;; (required for lsp-booster
  ;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
  ;;   "Try to parse bytecode instead of json."
  ;;   (or
  ;;    (when (equal (following-char) ?#)
  ;;      (let ((bytecode (read (current-buffer))))
  ;;        (when (byte-code-function-p bytecode)
  ;;          (funcall bytecode))))
  ;;    (apply old-fn args)))
  ;; (advice-add (if (progn (require 'json)
  ;;                        (fboundp 'json-parse-buffer))
  ;;                 'json-parse-buffer
  ;;               'json-read)
  ;;             :around
  ;;             #'lsp-booster--advice-json-parse)

  ;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  ;;   "Prepend emacs-lsp-booster command to lsp CMD."
  ;;   (let ((orig-result (funcall old-fn cmd test?)))
  ;;     (if (and (not test?)                             ;; for check lsp-server-present?
  ;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
  ;;              lsp-use-plists
  ;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
  ;;              (executable-find "emacs-lsp-booster"))
  ;;         (progn
  ;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
  ;;             (setcar orig-result command-from-exec-path))
  ;;           (message "Using emacs-lsp-booster for %s!" orig-result)
  ;;           (cons "emacs-lsp-booster" orig-result))
  ;;       orig-result)))
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;; )
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
        lsp-pylsp-plugins-mypy-enabled t
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

(use-package lsp-java
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :config
  (setq lsp-java-format-enabled t
        lsp-java-import-gradle-enabled t)
  :hook (java-ts-mode . lsp-deferred))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
(use-package dap-mode)

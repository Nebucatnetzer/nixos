;; -*- lexical-binding: t; -*-
(use-package eglot-mode
  :ensure nil
  :config
  (setq
   eglot-autoshutdown t
   eldoc-echo-area-use-multiline-p nil
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs `(ansible-mode . '("ansible-language-server" "--stdio")))
  :bind
  (:map eglot-mode-map
        ("C-c C-l r" . eglot-rename))
  :hook
  ((ansible-mode . eglot-ensure))
  :commands (eglot eglot-code-actions eglot-rename))

;; https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
  :vc (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package lsp-java
  :after (eglot-mode)
  :demand t
  :hook (java-ts-mode . eglot-ensure))

;; optionally if you want to use debugger
(use-package dap-mode)

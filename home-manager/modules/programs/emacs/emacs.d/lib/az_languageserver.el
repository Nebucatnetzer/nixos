;; -*- lexical-binding: t; -*-
(use-package flymake-ansible-lint
  :ensure t
  :commands flymake-ansible-lint-setup
  :hook ((ansible-mode . flymake-ansible-lint-setup)
         (ansible-mode . flymake-mode)))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package eglot-mode
  :ensure nil
  :config
  (setq
   eglot-autoshutdown t
   eldoc-echo-area-use-multiline-p nil
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024))
  :bind
  (:map eglot-mode-map
        ("C-c C-r" . eglot-rename))
  :commands (eglot eglot-code-actions eglot-rename))

;; https://github.com/jdtsmith/eglot-booster
(unless (package-installed-p 'eglot-booster)
  (package-vc-install '(eglot-booster
                        :url "https://github.com/jdtsmith/eglot-booster"
                        )))
(use-package eglot-booster
  :ensure nil
  :after eglot
  :config
  (eglot-booster-mode))

(use-package lsp-java
  :after (eglot-mode)
  :demand t
  :hook (java-ts-mode . eglot-ensure))

;; optionally if you want to use debugger
(use-package dap-mode)

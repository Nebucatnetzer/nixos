;; -*- lexical-binding: t; -*-

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Opens every file with a tree-sitter mode when its grammar is installed.
;; Nix provides the grammars; treesit-auto-install stays nil, so nothing downloads.
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;; Emacs 30.2 lists keywords that tree-sitter-php 0.24 no longer has as
;; tokens ("callable", "from"). One unknown name breaks the whole keyword
;; query and leaves PHP buffers uncoloured. Keep only the keywords the
;; installed grammar accepts; php-ts-mode probes its grammar the same way.
(use-package php-ts-mode
  :defer t
  :config
  (setq php-ts-mode--keywords
        (seq-filter #'az-php-keyword-known-p php-ts-mode--keywords)))

(defun az-php-keyword-known-p (keyword)
  "Return t if the PHP grammar has a token for KEYWORD."
  ;; EAGER t compiles at once, so an unknown name signals an error here.
  (condition-case nil
      (progn
        (treesit-query-compile 'php (format "%S @keyword" keyword) t)
        t)
    (treesit-query-error nil)))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package format-all
  :hook
  ((prog-mode . format-all-ensure-formatter)
   (ansible-mode . format-all-ensure-formatter)
   (yaml-ts-mode . format-all-ensure-formatter)
   (markdown-mode . format-all-ensure-formatter)
   (markdown-mode . format-all-mode)
   (prog-mode . format-all-mode))
  :bind (:map az-map ("C-f" . format-all-buffer))
  :config
  (define-format-all-formatter docformatter
    (:executable "docformatter")
    (:install)
    (:languages "Python")
    (:features)
    (:format (format-all--buffer-easy executable "-")))
  (setopt format-all-show-errors 'errors)
  ;; Only languages that differ from `format-all-default-formatters'.
  ;; format-all-formatters is buffer local, so set its default value.
  (setq-default format-all-formatters
                '(("C#" clang-format)
                  ("Haskell" ormolu)
                  ("Nix" nixfmt)
                  ("Python" (isort) (docformatter "--black") (black))
                  ("Shell" (shfmt "-i" "4")))))

(use-package haskell-mode
  :hook
  ((haskell-mode . eglot-ensure)
   (haskell-literate-mode . eglot-ensure)
   (haskell-mode . (lambda ()
                     (setq tab-width 2
                           haskell-indentation-layout-offset 2
                           haskell-indentation-starter-offset 2)))))

(use-package flymake-ansible-lint
  :commands flymake-ansible-lint-setup
  :hook ((ansible-mode . flymake-ansible-lint-setup)
         (ansible-mode . flymake-mode)))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package flymake
  :hook (sh-base-mode . flymake-mode))

(use-package eglot
  :config
  (setopt eglot-autoshutdown t
          eldoc-echo-area-use-multiline-p nil
          gc-cons-threshold 100000000
          read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
  :bind (:map az-map ("C-r" . eglot-rename))
  :commands (eglot eglot-code-actions eglot-rename))

;; https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))

(use-package hurl-mode
  :defer t)

(use-package jq-mode)

(use-package magit
  :demand t
  :commands magit-status
  :bind
  ("<f10>" . magit-status)
  :hook (git-commit-setup . flyspell-mode)
  :config
  (setopt magit-diff-refine-hunk (quote all)
          magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
          magit-save-repository-buffers 'dontask))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook
  ((nix-ts-mode . eglot-ensure)
   (nix-ts-mode . (lambda () (setq tab-width 2)))))

(use-package powershell
  :mode
  (("\\.ps1\\'" . powershell-mode)
   ("\\.psm1\\'" . powershell-mode)))

(use-package project
  :defer t
  :config
  ;; A directory with one of these files is a project, even without git.
  ;; .projectile keeps the directories marked for projectile working.
  (setopt project-vc-extra-root-markers '(".project" ".projectile"))
  (dolist (directory '("~/git_repos/projects/" "~/git_repos/work/"))
    (when (file-directory-p directory)
      (project-remember-projects-under directory))))

(use-package python
  :config
  (setopt python-shell-interpreter "python3"
          flymake-pylint-executable "pylint")
  :hook (python-ts-mode . eglot-ensure))

(use-package python-pytest
  :bind (:map python-ts-mode-map
              :package python
              ("C-c C-t" . python-pytest-run-def-at-point-treesit)
              ("C-c t" . python-pytest-dispatch)))

(defun az-python-flymake-setup ()
  "Add pylint and ruff after eglot has set its own flymake backend."
  (when (derived-mode-p 'python-base-mode)
    (pylint-setup-flymake-backend)
    (flymake-ruff-load)))

(add-hook 'eglot-managed-mode-hook #'az-python-flymake-setup)

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :hook
  (web-mode . (lambda ()
                (setq web-mode-markup-indent-offset 2
                      web-mode-css-indent-offset 2
                      web-mode-code-indent-offset 4)))
  :config)

(use-package ansible
  :after yaml-ts-mode
  :config (add-hook 'yaml-ts-mode-hook '(lambda () (ansible-mode 1))))

;; display the name of the function we are in the status bar
(use-package which-func
  :config
  (which-function-mode 1))

;; yaml-ts-mode derives from text-mode, so prog-mode does not cover it
(use-package display-fill-column-indicator
  :hook (prog-mode yaml-ts-mode))

(use-package js
  :defer t
  :custom (js-indent-level 2))

(use-package typescript-ts-mode
  :defer t
  :custom (typescript-ts-mode-indent-offset 2))

(use-package css-mode
  :defer t
  :custom (css-indent-offset 2))

(use-package go-ts-mode
  :hook (go-ts-mode . indent-tabs-mode))

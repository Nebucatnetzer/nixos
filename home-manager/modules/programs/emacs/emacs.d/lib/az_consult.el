;; -*- lexical-binding: t; -*-
;; https://github.com/minad/consult
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c j" . consult-git-grep))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (setq consult-preview-key 'any)

  )

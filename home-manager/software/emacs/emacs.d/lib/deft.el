(use-package deft
  :ensure nil
  :bind ("<f5>" . deft)
  :commands (deft)
  :config
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (setq deft-extensions '("md")
        deft-default-extension "md"
        deft-markdown-mode-title-level 1
        deft-auto-save-interval 300.0
        deft-file-limit 50
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-recursive t)
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "_")
          (case-fn . downcase)))
  (setq deft-directory "~/nextcloud/10_documents/"))

(use-package zetteldeft
  :after deft
  :config (zetteldeft-set-classic-keybindings)
  (setq zetteldeft-link-indicator "[["
        zetteldeft-link-suffix "]]")
  (setq zetteldeft-title-prefix "# "))

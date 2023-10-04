(when (boundp 'enable-notes)
  (use-package deft
    :ensure nil
    :bind ("<f5>" . open-notes)
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
          deft-strip-summary-regexp (concat "\\("
                                            "[\n\t]" ;; blank
                                            "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                                            "^---\\(?:\n.*\\)*---.*$"
                                            "\\)")

          deft-recursive t)
    (setq deft-file-naming-rules
          '((noslash . "-")
            (nospace . "_")
            (case-fn . downcase)))
    (setq deft-directory "~/nextcloud/10_documents/")
    (add-hook 'deft-mode-hook (lambda() (display-line-numbers-mode -1)))

    ;; A function to create a persp for my notes
    (defun open-notes ()
      "Create a notes perspective and open deft"
      (interactive)
      (persp-switch "notes")
      (deft))))

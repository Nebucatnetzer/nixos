;; -*- lexical-binding: t; -*-
(when (boundp 'enable-notes)
  (use-package denote
    :init
    (require 'denote-org-extras)
    (require 'denote-journal-extras)
    :bind
    (("C-c n r" . denote-rename-file)
     ("C-c n p" . az-note-from-region)
     ("C-c n t" . denote-journal-extras-new-or-existing-entry)
     ("C-c n l" . denote-link)
     ("C-c n n" . denote-subdirectory))
    :config
    (defun az-note-from-region (beg end)
      "Create note whose contents include the text between BEG and END. Prompt
    for title and keywords of the new note."
      (interactive "r")
      (if-let (((region-active-p))
               (text
                (buffer-substring-no-properties beg end)))
          (progn (denote
                  (denote-title-prompt) (denote-keywords-prompt)) (insert text))
        (user-error
         "No region is available")))
    (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
    (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
    (setq denote-rename-buffer-mode 1
          denote-journal-extras-directory (concat denote-directory "99_archive/" (format-time-string "%Y") "/journal/")
          denote-journal-extras-title-format 'day-date-month-year
          denote-file-type "org"
          denote-directory "~/nextcloud/"
          denote-dired-directories (list denote-directory)
          denote-dired-directories-include-subdirectories t
          denote-org-front-matter "#+title: %s\n:preamble:\n#+date: %s\n#+filetags: %s\n#+identifier: %s\n#+author: Andreas Zweili\n#+setupfile: ~/nextcloud/99_archive/0000/settings/html_theme/setup/theme-readtheorg-local.setup\n#+latex_header: \input{~/nextcloud/99_archive/0000/settings/latex/style.tex}\n:end:\n\n"
          denote-yaml-front-matter "---\ntitle: %s\ndate: %s\ntags: %s\nidentifier: %S\n---\n\n")))

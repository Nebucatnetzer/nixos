;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-notes)
  (defvar-keymap az-notes-map
    :doc "Notes commands on C-l C-n.")
  (keymap-set az-map "C-n" az-notes-map)

  (defun az-open-notes ()
    "Toggle the notes perspective.
  On notes, go back to the previous perspective. Otherwise switch to
  notes and create it with a dired buffer in the notes directory."
    (interactive)
    (cond
     ((string= (persp-current-name) "notes")
      (persp-prev))
     ((member "notes" (persp-names))
      (persp-switch "notes"))
     (t
      ;; denote-directory only exists after denote and its :config load.
      (require 'denote)
      (persp-switch "notes")
      (dired denote-directory))))

  (use-package denote
    :bind
    (("<f5>" . az-open-notes)
     :map az-notes-map
     ("C-r" . denote-rename-file)
     ("C-p" . az-note-from-region)
     ("C-l" . denote-link)
     ("C-n" . denote-subdirectory))
    :config
    (defvar az-denote-org-front-matter
      (concat "#+title: %s\n:preamble:\n"
              "#+date: %s\n"
              "#+filetags: %s\n"
              "#+identifier: %s\n"
              "#+author: Andreas Zweili\n"
              "#+setupfile: " az-org-html-setup-file "\n"
              "#+latex_header: \\input{" az-org-latex-style-file "}\n"
              ":end:\n\n"))
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
          denote-file-type "org"
          denote-directory az-nextcloud-dir
          denote-dired-directories (list denote-directory)
          denote-dired-directories-include-subdirectories t
          denote-excluded-directories-regexp "20_pictures\\|21_auto_uploads\\|22_avatars\\|23_ich\\|24_wallpapers\\|30_keepass\\|40_books\\|90_public\\|98_zotero"
          denote-org-front-matter az-denote-org-front-matter
          denote-yaml-front-matter "---\ntitle: %s\ndate: %s\ntags: %s\nidentifier: %S\n---\n\n"))

  (use-package denote-org)

  (use-package denote-journal
    :bind
    (:map az-notes-map
          ("C-t" . denote-journal-new-or-existing-entry))
    :config
    ;; The year here is computed once, at load time.  az_org_log.el advises
    ;; the journal entry points to recompute it, so a daemon running past New
    ;; Year does not keep writing into the previous year's directory.
    (setopt denote-journal-directory (concat denote-directory "99_archive/" (format-time-string "%Y") "/journal/")
            denote-journal-title-format 'day-date-month-year)))

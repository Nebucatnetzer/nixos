;; -*- lexical-binding: t; -*-
(when (boundp 'enable-notes)
  (use-package denote
    :bind
    (("C-c n r" . denote-rename-file)
     ("C-c n p" . az-note-from-region)
     ("C-c n t" . az-denote-journal)
     ("C-c n n" . denote))
    :config
    (defun az-denote-journal ()
      "Create an entry tagged 'journal' with the date as its title. \
       If a journal for the current day exists, visit it.\
       If multiple entries exist, prompt with completion for a choice between them.
       Else create a new file."
      (interactive)
      (let* ((today (format-time-string "%Y-%m-%d"))
             (string (denote-sluggify-title today))
             (files (denote-directory-files-matching-regexp string)))
        (cond
         ((> (length files) 1)
          (find-file (completing-read "Select file: " files nil :require-match)))
         (files
          (find-file (car files)))
         (t
          (denote
           today
           '("journal"))))))
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
    (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
    (setq denote-rename-buffer-mode 1
          denote-excluded-directories-regexp "99_archive"
          denote-file-type "org"
          denote-directory "~/nextcloud/10_documents/"
          denote-org-front-matter "#+title: %s\n#+date: %s\n#+filetags: %s\n#+identifier: %s\n#+author: Andreas Zweili\n\n"
          denote-yaml-front-matter "---\ntitle: %s\ndate: %s\ntags: %s\nidentifier: %S\n---\n\n")))

(when (boundp 'enable-notes)
  (use-package denote
    :bind
    (("C-c n r" . denote-rename-file)
     ("C-c n p" . az-note-from-region)
     ("C-c n n" . denote))
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
    (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
    (setq denote-rename-buffer-mode 1
          denote-excluded-directories-regexp "99_archive"
          denote-file-type "markdown-yaml"
          denote-directory "~/nextcloud/10_documents/"
          denote-yaml-front-matter "---\ntitle: %s\ndate: %s\ntags: %s\nidentifier: %S\n---\n\n")))

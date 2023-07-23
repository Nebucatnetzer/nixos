(use-package denote
  :bind
  (("C-c C-n r" . denote-rename-file)
   ("C-c C-n p" . az-denote-create-new-note-from-region)
   ("C-c C-n n" . denote-create-file))
  :config
  (defun az-denote-create-new-note-from-region (beg end)
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
  (setq denote-file-type "markdown-yaml"
        denote-directory "~/nextcloud/10_documents/"))

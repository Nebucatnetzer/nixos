(use-package denote
  :bind
  (("C-c C-n r" . denote-rename-file)
   ("C-c C-n n" . denote-create-file))
  :config
  (setq denote-file-type "markdown-yaml"
        denote-directory "~/nextcloud/10_documents/"
        denote-excluded-directories-regexp "99_archive/*"))

(use-package denote
  :bind
  (("C-c n r" . denote-rename-file)
   ("C-c n n" . denote-create-file))
  :config
  (setq denote-file-type "markdown-yaml"
        denote-directory "~/nextcloud/10_documents/"))

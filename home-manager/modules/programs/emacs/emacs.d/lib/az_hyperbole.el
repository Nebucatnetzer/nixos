(use-package hyperbole
  :config
  (hyperbole-mode 1)

  ;; https://emacs.stackexchange.com/a/55693/17595
  (let ((file "~/nextcloud/10_documents/02_projects/projects.el"))
    (when (file-exists-p file)
      (setq initial-scratch-message
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))))

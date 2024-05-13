;; -*- lexical-binding: t; -*-
(use-package hyperbole
  :config
  ;; define the local variable az-user-dir to a path and then check if the directory exists
  ;; if the directory exists then set the variable hbmap:dir-user to the path
  (let ((az-user-dir (expand-file-name "~/nextcloud/10_documents/99_archive/0000/hyperbole/")))
    (when (file-exists-p az-user-dir)
      (setq hbmap:dir-user az-user-dir)))

  (define-key hyperbole-mode-map (kbd "C-c /") nil)
  (hyperbole-mode 1))

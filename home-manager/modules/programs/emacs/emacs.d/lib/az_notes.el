;; -*- lexical-binding: t; -*-
(when (boundp 'enable-notes)
  (use-package emacs
    :ensure nil
    :bind ("<f5>" . open-notes))

  ;; A function to create a persp for my notes
  (defun open-notes () "Create a notes perspective and open dired in the notes
directory." (interactive) (persp-switch "notes") (dired denote-directory)))

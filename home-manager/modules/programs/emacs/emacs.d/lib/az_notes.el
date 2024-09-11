;; -*- lexical-binding: t; -*-
(when (boundp 'enable-notes)
  (use-package emacs
    :ensure nil
    :bind ("<f5>" . open-notes))

  ;; A function to create a persp for my notes
  (defun open-notes ()
    "Create a notes perspective and open dired in the notes directory."
    (interactive)
    (persp-switch "notes")
    (dired denote-directory))


  (defun az-buffer-file-path-as-url ()
    "Return the current buffer's file path as a command and copy it to the clipboard."
    (interactive)
    (let ((file-path (buffer-file-name)))
      (if file-path
          (let ((url (concat "`emacsclient " "\"" file-path "\"`")))
            ;; Copy the URL to the clipboard
            (kill-new url)
            ;; Display the URL in the minibuffer
            (message "Buffer file URL: %s (Copied to clipboard)" url)
            ;; Return the URL
            url)
        (message "Current buffer is not visiting a file")
        nil))))

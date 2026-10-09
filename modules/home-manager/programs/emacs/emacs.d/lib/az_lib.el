;; -*- lexical-binding: t; -*-
(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun az-wsl-p ()
  "Return non-nil when Emacs runs inside WSL."
  (file-exists-p "/etc/wsl.conf"))

(defun az-prose-editing ()
  "Centre the text with olivetti and stop hard line breaks."
  (olivetti-mode 1))

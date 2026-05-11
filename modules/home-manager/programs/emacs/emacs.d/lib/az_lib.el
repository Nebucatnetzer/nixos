;; -*- lexical-binding: t; -*-
(defun az-buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))

(defun az-generic-setup ()
  "turn off `linum-mode' when there are more than 5000 lines."
  (if (az-buffer-too-big-p) (display-line-numbers-mode -1)))

;; disable background in terminal
(defun az-on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun az-on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun is-windows-p ()
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)
      (eq system-type 'cygwin)))

(defun is-bsd-p ()
  (eq system-type 'gnu/kfreebsd))

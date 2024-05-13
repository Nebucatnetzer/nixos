;; -*- lexical-binding: t; -*-
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun az-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

;; taken from here: https://zck.org/emacs-move-file
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defun az-center-buffer ()
  (interactive)
  (let ((margin-size (/ (- (frame-width) 80) 2)))
    (set-window-margins nil margin-size margin-size)))

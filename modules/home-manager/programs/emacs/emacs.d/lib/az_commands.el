;; -*- lexical-binding: t; -*-
(defun az-center-buffer ()
  (interactive)
  (let ((margin-size (/ (- (frame-width) 80) 2)))
    (set-window-margins nil margin-size margin-size)))

(defun az-copy-all ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun az-kill-dired-buffers ()
  "Kill all buffers in Dired mode."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (eq major-mode 'dired-mode))
      (kill-buffer buffer))))

;; taken from: https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(defun az-keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))

(defun az-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun az-insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun az-insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun az-insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

;; taken from here: https://zck.org/emacs-move-file
(defun az-move-file (new-location)
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

(defun az-open-notes ()
  "Toggle the notes perspective. If already on 'notes', switch to the previous
perspective. If 'notes' exists but isn't active, switch to it. Otherwise,
create it and open dired in the notes directory."
  (interactive)
  (cond
   ((string= (persp-current-name) "notes")
    (persp-prev))

   ((member "notes" (persp-names))
    (persp-switch "notes"))

   (t
    (persp-switch "notes")
    (dired denote-directory))))

;; Taken from here: https://www.emacswiki.org/emacs/Replace-in-buffer
(defun az-replace-in-buffer ()
  "Search and replace given string in current buffer."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setopt curr-word (buffer-substring-no-properties (mark) (point))
            old-string (read-string "Replace: " curr-word)
            new-string (read-string "With: " old-string))
    (query-replace old-string new-string nil (point-min) (point-max))))

(defun az-split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun az-split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun az-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun az-toggle-window-dedication ()
  "Toggle window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(defun az-toggle-window-split ()
  "Toggle between horizontal and vertical split when two windows are open."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

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

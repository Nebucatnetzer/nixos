;; -*- lexical-binding: t; -*-
;; a function to toggle the splits
(defun toggle-window-split ()
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

(define-key ctl-x-map "4" 'toggle-window-split)

(defun az-split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun az-split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; Prefixed the numbers to work better with god-mode
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'az-split-window-below-and-move-cursor)
(global-set-key (kbd "C-x C-3") 'az-split-window-right-and-move-cursor)
(global-set-key (kbd "C-x C-0") #'kill-buffer-and-window)

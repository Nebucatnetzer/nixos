;; keymap for buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; kill THIS buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; copy the complete buffer to the clipboard
(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(global-set-key (kbd "C-S-c") 'copy-all)

;; keybinding for new frame
(global-set-key (kbd "C-x N") 'make-frame)

;; switch to frame
(global-set-key (kbd "C-x O") 'other-frame)

;; kill frame
(global-set-key (kbd "C-x K") 'delete-frame)

(defun switch-to-minibuffer () "Switch to minibuffer window."
       (interactive) (if (active-minibuffer-window)
                         (select-window
                          (active-minibuffer-window)) (error "Minibuffer is not active")))

(bind-key "M-m" 'switch-to-minibuffer)

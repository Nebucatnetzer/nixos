;; -*- lexical-binding: t; -*-
;; todo: switch-buffer should use consult-projectile

(use-package ace-window
  :init
  ;; Redefine the list as I don't have transpose-frame installed atm.

  ;; inspired by: https://github.com/SalOrak/dotfiles/blob/32a48e11964e8c501c9e9bf0f63b50a6034c04cb/files/emacs/plugins/ace-window.el#L12
  (defun az/aw-consult-switch-buffer (window)
    "Switch the buffer of the selected window using `ace-window' ala `consult-projectile'"
    (aw-switch-to-window window)
    (consult-projectile))

  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u sk/aw-consult-switch-buffer "Switch Buffer Other Window")
      (?e aw-execute-command-other-window "Execute Command Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?t az-tear-off-window "Create frame with window")
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help)))
  :config
  (defun az-tear-off-window (window)
    "Select a window with ace-window and tear it off the frame.

This displays the window in a new frame, see `tear-off-window'."
    (when-let ((win (select-window window))
               (buf (window-buffer win))
               (frame (make-frame)))
      (select-frame frame)
      (pop-to-buffer-same-window buf)
      (delete-window win)))

  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  (keymap-global-set "M-O"  'ace-window-prefix)

  (defun ace-window-one-command ()
    (interactive)
    (let ((win (aw-select " ACE")))
      (when (windowp win)
        (with-selected-window win
          (let* ((command (key-binding
                           (read-key-sequence
                            (format "Run in %s..." (buffer-name)))))
                 (this-command command))
            (call-interactively command))))))

  (keymap-global-set "C-x O" 'ace-window-one-command)

  ;; General ace settings
  (global-set-key (kbd "M-o") 'ace-window)
  (setopt ace-window-display-mode t
          aw-dispatch-always t
          aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          ace-window-mode t)
  )

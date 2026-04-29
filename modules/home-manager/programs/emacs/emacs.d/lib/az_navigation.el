;; -*- lexical-binding: t; -*-
;; todo: switch-buffer should use consult-projectile
(use-package ace-window
  :init
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
      (?u az/aw-consult-switch-buffer "Switch Buffer Other Window")
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

  ;;(keymap-global-set "M-O"  'ace-window-prefix)

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

;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(use-package avy
  :config
  (setopt avy-keys '(?a ?s ?d ?f ?g ?j ?l ?o
                        ?v ?b ?n ?, ?/ ?u ?p ?e
                        ?c ?q
                        )
          avy-all-windows 'all-frames)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

;; https://github.com/oantolin/embark
(use-package embark
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; hyperbole
(use-package hyperbole
  :init
  (setopt hywiki-directory "~/.emacs.d/hywiki")
  :config
  (hyperbole-mode 1)
  (define-key hyperbole-mode-map (kbd "M-o"  ) nil)
  (define-key hyperbole-mode-map (kbd "M-S-RET"  ) nil)
  )

;; ultra-scroll
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  :init
  (setopt scroll-conservatively 101 ; important!
          scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

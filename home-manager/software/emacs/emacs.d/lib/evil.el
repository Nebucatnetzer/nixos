;; evil-mode allows to use vim keybindings
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  :config
  ;; Start these modes in emacs mode
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  (setq global-evil-search-highlight-persist t)

  ;; Add vim keybindings to the bookmark menu
  (evil-add-hjkl-bindings bookmark-bmenu-mode-map 'emacs
    ;; (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Add vim keybindings to the ibuffer
  (evil-add-hjkl-bindings ibuffer-mode-map 'emacs
    ;; (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Add vim keybindings to the package mode
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    ;; (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-add-hjkl-bindings helpful-mode-map 'emacs
    ;; (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; vim keybindings for mu4e
  (evil-add-hjkl-bindings mu4e-headers-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-add-hjkl-bindings mu4e-view-mode-map 'emacs
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (defun az-dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)))

  ;; evil keybindings for dired
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "h" 'az-dired-up-directory)
    (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
    (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
    (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
    (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
    (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
    (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
    (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
    (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous))

  ;; evil keybindings for window movement
  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-j") 'windmove-down)

  (evil-mode 1))

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(calendar calc dired ediff magit)))

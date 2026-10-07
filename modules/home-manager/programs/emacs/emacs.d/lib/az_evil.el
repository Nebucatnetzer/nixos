;; -*- lexical-binding: t; -*-
;; evil-mode allows to use vim keybindings
(use-package evil
  :init
  (setopt evil-undo-system 'undo-redo
          evil-want-C-i-jump nil ;; otherwhie TAB doesn't work in org mode
          evil-want-integration t ;; required by evil-collection
          evil-want-keybinding nil) ;; required by evil-collection

  :config
  (general-def :states 'motion
    "/" 'consult-line)

  (evil-mode 1))

(define-key evil-normal-state-map [escape] 'az-keyboard-quit)
(define-key evil-visual-state-map [escape] 'az-keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after (evil magit)
  :config
  (evil-collection-init)

  ;; evil keybindings for dired
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map "q" 'az-kill-dired-buffers)
    (evil-define-key 'normal dired-mode-map "l" 'dired-find-file)
    (evil-define-key 'normal dired-mode-map (kbd "SPC") 'god-execute-with-current-bindings))

  (with-eval-after-load 'locate
    (define-key locate-mode-map (kbd "SPC") 'god-execute-with-current-bindings))

  (with-eval-after-load 'org-agenda
    (when (bound-and-true-p enable-org)
      (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
        (kbd "n")   'evil-search-next
        (kbd "N")   'evil-search-previous
        (kbd "C-d") 'evil-scroll-down
        (kbd "C-u") 'evil-scroll-up
        (kbd "c")   'org-capture
        (kbd "$")   'evil-end-of-line
        (kbd "SPC") 'god-execute-with-current-bindings))))

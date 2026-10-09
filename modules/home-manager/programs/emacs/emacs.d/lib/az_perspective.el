;; -*- lexical-binding: t; -*-
;; Keys are C-<letter>, so god-mode reaches C-l C-x C-s as SPC l x s.
(defvar-keymap az-persp-map
  :doc "Perspective commands on C-l C-x."
  "C-s" #'persp-switch
  "C-c" #'persp-kill
  "C-r" #'persp-rename)
(keymap-set az-map "C-x" az-persp-map)

(use-package perspective
  :after consult
  :bind
  (("C-x b" . persp-ibuffer)
   ("C-x k" . persp-kill-buffer*))
  :custom
  ;; Perspective's own map uses plain letters, which god-mode cannot reach.
  (persp-mode-prefix-key nil)
  (persp-suppress-no-prefix-key-warning t)
  :config
  (consult-customize consult-source-buffer :hidden t :default nil)
  ;; perspective binds to mouse click instead of release which then sometimes causes the org clock drawer to fire.
  ;; Rebinding it to mouse release.
  (keymap-unset persp-mode-line-map "<mode-line> <down-mouse-1>" t)
  (keymap-set persp-mode-line-map "<mode-line> <mouse-1>" #'persp-mode-line-click)

  (add-to-list 'consult-buffer-sources persp-consult-source)
  :init
  ;; Default file for M-x persp-state-save and persp-state-load.
  (setopt persp-state-default-file
          (expand-file-name "persp-session" user-emacs-directory))
  (persp-mode))

;; Amx, an alternative interface for M-x in Emacs
;; https://github.com/DarwinAwardWinner/amx
;; It shows the keyboard commands assigned to the command
(use-package amx
  :config
  (amx-mode t))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character
        hightlight-indentation-mode nil
        highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "gray")
  (set-face-foreground 'highlight-indent-guides-character-face "gray")
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; change the colours of parenthesis the further out they are
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; smooth scrolling
(use-package smooth-scrolling
  :config
  (setq scroll-margin 1
        scroll-conservatively 9999
        scroll-step 1))

;; highlight bad whitespace
(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail tabs trailing))
  (set-face-attribute 'whitespace-line nil :foreground "#af005f")
  (global-whitespace-mode t))

;; disable menu and toolbar
(tool-bar-mode -1)
;;(menu-bar-mode -99)
(when (boundp 'enable-scroll-bar)
  (scroll-bar-mode -1))

;; Proper line wrapping
(global-visual-line-mode 1)

;; Disable fringe because I use visual-line-mode
(when (boundp 'disable-fringe)
  (set-fringe-mode '(0 . 0)))

;; Disable splash screen
(setq inhibit-splash-screen t)

;; disable tooltips
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; disable or reconfigure prompts
(fset 'yes-or-no-p 'y-or-n-p) ;; remap yes or no to y or n

;; just create buffers don't ask
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

(when (boundp 'enable-font)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 140
                      :weight 'normal
                      :width 'normal))

(toggle-frame-maximized)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                ;; "  "
                ;; mode-line-modes
                "   "
                mode-line-misc-info
                ;; battery-mode-line-string
                ;; mode-line-end-spaces
                ))

(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

(setq inhibit-compacting-font-caches t)

;; hide temporary buffers
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-filter-by-name "^[^*]")))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . python-mode)
                 (mode . web-mode)
                 (mode . php-mode)
                 (mode . csharp-mode)
                 (mode . javascript-mode)
                 (mode . sql-mode)
                 (mode . powershell-mode)
                 (mode . nix-mode)
                 (mode . yaml-mode)
                 (mode . emacs-lisp-mode)))
               ;; etc
               ("Dired"
                (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(when (boundp 'enable-emojis)
  (when (is-linux-p)
    (set-fontset-font t nil "Symbola" nil 'prepend)))

(setq column-number-mode 1)

;; improve performance with large files (
(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))
(defun generic-setup ()
  ;; turn off `linum-mode' when there are more than 5000 lines
  (if (buffer-too-big-p) (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook 'generic-setup)
(add-hook 'text-mode-hook 'generic-setup)
;; )

(setq use-short-answers t)

;; Matches parentheses and such in every mode
(show-paren-mode 1)

;; pair parentheses
(electric-pair-mode 1)

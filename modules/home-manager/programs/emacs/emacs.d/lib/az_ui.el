;; -*- lexical-binding: t; -*-
(use-package highlight-indent-guides
  :config
  (setopt highlight-indent-guides-method 'character
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

(use-package alabaster-themes
  :config
  ;; Emacs only probes the terminal background under TERM=xterm*. Under tmux-256color
  ;; it does not, and would guess dark, which breaks this light theme in a tty.
  (setopt frame-background-mode 'light)
  (mapc #'frame-set-background-mode (frame-list))

  ;; Terminals get #000000 as palette color 0 and often show bold palette colors
  ;; as bright instead of bold. Near black is sent as 24-bit color.
  (setopt alabaster-themes-light-bg-palette-overrides
          '((fg-main "#0a0a0a")
            (fg-intense "#0a0a0a")
            (fg-mode-line "#0a0a0a")
            (fg-region "#0a0a0a")))

  (load-theme 'alabaster-themes-light-bg t)
  (custom-set-faces
   '(line-number ((((type tty)) :foreground "#777777" :background "#f5f5f5")))
   '(line-number-current-line ((((type tty)) :foreground "#0a0a0a" :background "#ffffff" :weight bold)))))


;; highlight bad whitespace
(use-package whitespace
  :config
  (setopt whitespace-style '(face tabs trailing))
  (set-face-attribute 'whitespace-line nil :foreground "#af005f")
  (global-whitespace-mode t))

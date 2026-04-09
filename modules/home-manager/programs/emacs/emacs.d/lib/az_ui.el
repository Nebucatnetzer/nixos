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

;; smooth scrolling
(use-package smooth-scrolling
  :config
  (setopt scroll-margin 1
          scroll-conservatively 9999
          scroll-step 1))

(when (boundp 'enable-color-theme)
  ;; load solarized color theme
  (use-package alabaster-themes
    :config
    (load-theme 'alabaster-themes-light-bg t)))

;; highlight bad whitespace
(use-package whitespace
  :config
  (setopt whitespace-style '(face lines-tail tabs trailing))
  (set-face-attribute 'whitespace-line nil :foreground "#af005f")
  (global-whitespace-mode t))

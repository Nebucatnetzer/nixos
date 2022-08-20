;; smooth scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 1
        scroll-conservatively 9999
        scroll-step 1))

;; change the colours of parenthesis the further out they are
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character
        hightlight-indentation-mode nil
        highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "gray")
  (set-face-foreground 'highlight-indent-guides-character-face "gray")
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

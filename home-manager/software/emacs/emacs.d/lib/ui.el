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

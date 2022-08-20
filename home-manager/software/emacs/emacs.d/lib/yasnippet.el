;; enable yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; enable yasnippet
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-global-mode 1))

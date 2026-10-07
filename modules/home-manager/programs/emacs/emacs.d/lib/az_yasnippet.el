;; -*- lexical-binding: t; -*-
;; enable yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; adds the snippet collection to yas-snippet-dirs when loaded
(use-package yasnippet-snippets
  :after yasnippet)

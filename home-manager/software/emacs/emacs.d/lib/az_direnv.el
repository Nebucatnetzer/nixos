(use-package direnv
  :config
  (direnv-mode)
  :hook
  ((after-init . direnv-mode)
   (lsp-before-initialize-hook . direnv-update-environment)))

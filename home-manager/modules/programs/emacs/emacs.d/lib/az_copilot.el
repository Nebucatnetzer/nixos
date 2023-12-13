(use-package dash
  :defer t)

(use-package editorconfig
  :defer t)

(use-package s
  :defer t)

(use-package copilot
  :load-path "~/.nixos/home-manager/modules/programs/emacs/emacs.d/lib/copilot"
  :hook (prog-mode-hook . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package dash
  :defer t)

(use-package editorconfig
  :defer t)

(use-package s
  :defer t)

(use-package copilot
  :load-path "~/.nixos/home-manager/modules/programs/emacs/emacs.d/lib/copilot"
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-hook 'yaml-mode-hook 'copilot-mode)
  (add-to-list 'copilot-disable-display-predicates #'company--active-p)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
(use-package python-mode
  :ensure t
  :defer t
  :config
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq eldoc-idle-delay 1)
  (add-hook 'python-mode-hook (lambda () (highlight-indentation-mode -1)))
  (elpy-enable))

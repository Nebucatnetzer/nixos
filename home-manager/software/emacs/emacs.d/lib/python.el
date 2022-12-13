(use-package python-mode
  :defer t
  :config
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :defer t
  :config
  (setq elpy-test-runner 'elpy-test-pytest-runner
        elpy-formatter 'black
        elpy-rpc-virtualenv-path 'system
        eldoc-idle-delay 1)
  (add-hook 'python-mode-hook (lambda () (highlight-indentation-mode -1)))
  :init
  (advice-add 'python-mode :before 'elpy-enable))

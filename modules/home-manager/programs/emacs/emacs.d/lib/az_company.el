(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setopt company-tooltip-minimum-width 25
          company-tooltip-align-annotations t
          company-dabbrev-downcase nil
          company-idle-delay 0)
  (global-company-mode 1))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setopt company-tooltip-minimum-width 25
          company-tooltip-align-annotations t
          company-dabbrev-downcase nil
          company-idle-delay 0
          ;; company-files is grouped with the others so it merges
          ;; candidates instead of being shadowed by a backend (e.g.
          ;; company-capf) that claims the prefix first.
          company-backends '((company-files company-capf company-dabbrev-code company-keywords)
                              company-dabbrev))
  (global-company-mode 1))

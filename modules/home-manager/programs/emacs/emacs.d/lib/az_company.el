(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setopt company-tooltip-minimum-width 25
          company-tooltip-align-annotations t
          company-dabbrev-downcase nil
          ;; The default "\\sw" stops at _ and -, so snake_case and kebab-case
          ;; names come back as separate fragments.  Literal characters rather
          ;; than \\s_ keep org's emphasis markers (* / = ~ +) out of the
          ;; candidates; those carry symbol syntax too.  company-dabbrev-code
          ;; already matches whole symbols, so this only affects buffers that
          ;; fall through to company-dabbrev, meaning non prog-mode ones.
          company-dabbrev-char-regexp "\\sw\\|[_-]"
          company-idle-delay 0
          ;; company-files is grouped with the others so it merges
          ;; candidates instead of being shadowed by a backend (e.g.
          ;; company-capf) that claims the prefix first.
          company-backends '((company-files company-capf company-dabbrev-code company-keywords)
                              company-dabbrev))
  (global-company-mode 1))

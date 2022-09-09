;; enable magit a great git porcelain.
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("<f10>" . magit-status)
  :config
  (setq magit-diff-refine-hunk (quote all)))

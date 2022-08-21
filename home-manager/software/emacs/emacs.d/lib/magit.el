;; enable magit a great git porcelain.
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("<f10>" . magit-status))

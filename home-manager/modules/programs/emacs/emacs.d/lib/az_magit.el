;; -*- lexical-binding: t; -*-
;; enable magit a great git porcelain.
(use-package magit
  :demand t
  :commands magit-status
  :bind
  ("<f10>" . magit-status)
  :hook (git-commit-setup . flyspell-mode)
  :config
  (setq magit-diff-refine-hunk (quote all)
        magit-save-repository-buffers 'dontask))

;; -*- lexical-binding: t; -*-
(when (boundp 'enable-color-theme)
  ;; load solarized color theme
  (use-package solarized-theme
    :config
    (setq solarized-use-variable-pitch nil)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-high-contrast-mode-line t)
    (set-face-inverse-video 'region nil)
    (load-theme 'solarized-light t)))

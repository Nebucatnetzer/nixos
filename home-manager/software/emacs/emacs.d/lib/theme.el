;; Color theme
;; disable background in terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(when (boundp 'enable-color-theme)
  ;; load solarized color theme
  (use-package solarized-theme
    :ensure t
    :config
    (setq solarized-use-variable-pitch nil)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-high-contrast-mode-line t)
    (set-face-inverse-video 'region nil)
    (load-theme 'solarized-light t)))
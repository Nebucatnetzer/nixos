;; -*- lexical-binding: t; -*-
(when (boundp 'enable-pdf-tools)
  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    ;; turn off cua so copy works
    (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
    (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
    ;; keyboard shortcuts
    (define-key pdf-view-mode-map (kbd "C-w C-w") 'other-window)
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-page-command)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-page-command)
    (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
    (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)))

(when (boundp 'enable-pdf-tools)
  ;; improve the resolution of doc-view
  (setq doc-view-resolution 200))

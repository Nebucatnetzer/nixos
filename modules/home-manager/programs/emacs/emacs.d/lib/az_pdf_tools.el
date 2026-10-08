;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-pdf-tools)
  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
                ("j" . pdf-view-next-page-command)
                ("k" . pdf-view-previous-page-command)
                ("h" . pdf-annot-add-highlight-markup-annotation)
                ("t" . pdf-annot-add-text-annotation)
                ("D" . pdf-annot-delete))
    :config
    (with-demoted-errors "pdf-tools-install failed: %s"
      (pdf-tools-install))
    (setq-default pdf-view-display-size 'fit-page)))

;; doc-view is the fallback viewer when pdf-tools is off
(setopt doc-view-resolution 200)

(when (boundp 'enable-auctex)
  ;; auctex a greate plugin for latex writing
  (use-package latex
    :if (is-linux-p)
    :ensure auctex
    :mode ("\\.tex\\'" . latex-mode)
    :ensure-system-package
    (pdflatex . texlive-full)
    :config
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-electric-math (quote ("\\(" . "\\)")))
    (setq LaTeX-electric-left-right-brace t)
    (setq TeX-view-program-selection
          (quote
           (((output-dvi has-no-display-manager)
             "dvi2tty")
            ((output-dvi style-pstricks)
             "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "PDF Tools")
            (output-html "xdg-open"))))))

(when (boundp 'enable-auctex)
  (use-package company-auctex
    :if (is-linux-p)
    :ensure t
    :after auctex
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'company-auctex-init)))

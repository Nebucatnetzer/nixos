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
    :after auctex
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'company-auctex-init)))

(when (boundp 'enable-auctex)
  (defun LaTeX-collapse-table ()
    (interactive)
    (save-excursion
      (LaTeX-mark-environment)
      (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
        (replace-match " \\1"))))

  (defun LaTeX-align-environment (arg)
    (interactive "P")
    (if arg
        (LaTeX-collapse-table)
      (save-excursion
        (LaTeX-mark-environment)
        (align (region-beginning) (region-end))))))

(when (boundp 'enable-auctex)
  (defcustom LaTeX-inhibited-auto-fill-environments
    '("tabular" "tikzpicture") "For which LaTeX environments not to run auto-fill.")

  (defun LaTeX-limited-auto-fill ()
    (let ((environment (LaTeX-current-environment)))
      (when (not (member environment LaTeX-inhibited-auto-fill-environments))
        (do-auto-fill)))))

(when (boundp 'enable-auctex)
  (global-set-key (kbd "C-c f") 'LaTeX-align-environment)
  (setq auto-fill-function 'LaTeX-limited-auto-fill))

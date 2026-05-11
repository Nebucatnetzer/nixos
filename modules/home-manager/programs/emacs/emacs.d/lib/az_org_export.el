;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  (use-package org
    :config
    ;; org-export formats
    (setq org-export-backends (quote (beamer html latex md odt reveal)))

    (setopt org-startup-shrink-all-tables t

            org-html-html5-fancy t
            org-html-doctype "html5"

            ;; disable the Todo keywords in the export
            org-export-with-todo-keywords nil

            ;; disable the tags in the export
            org-export-with-tags nil

            ;; place captions below images
            org-latex-caption-above nil

            org-export-with-sub-superscripts nil ;; why is this needed?

            org-export-with-smart-quotes t

            org-export-headline-levels 5

            ;; options for beamer exports
            org-beamer-frame-level 2
            org-beamer-outline-frame-options ""
            org-beamer-outline-frame-title "Inhalt"
            org-beamer-theme "metropolis")

    ;; options for latex exports
    (setopt org-latex-classes
            (quote
             (("beamer" "\\documentclass{beamer}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
              ("article" "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
              ("report" "\\documentclass[11pt]{report}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
              ("book" "\\documentclass[11pt]{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
            org-latex-default-packages-alist nil
            org-latex-listings 'listings
            org-latex-title-command "\\maketitle\\newpage"
            org-latex-toc-command "\\tableofcontents\\newpage")

    ;; https://emacs.stackexchange.com/a/41619
    (defun az-markdown-convert-buffer-to-org ()
      "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
      (interactive)
      (let ((convert-command (format "pandoc -f markdown -t org -o %s"
                                     (concat (shell-quote-argument(file-name-sans-extension (buffer-file-name))) ".org"))))
        (message convert-command)
        (shell-command-on-region (point-min) (point-max)
                                 convert-command)))))

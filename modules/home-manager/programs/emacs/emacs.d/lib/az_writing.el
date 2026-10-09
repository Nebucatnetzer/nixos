;; -*- lexical-binding: t; -*-
(defun az-lang-tool ()
  "Load flymake-languagetool and start flymake."
  (interactive)
  (require 'flymake-languagetool)
  (flymake-languagetool-maybe-load)
  (flymake-mode 1))

(defvar-keymap az-spell-map
  :doc "Spell checking on C-l C-w."
  "C-d" #'ispell-change-dictionary
  "C-s" #'ispell
  "C-l" #'az-lang-tool)
(keymap-set az-map "C-w" az-spell-map)

(use-package text-mode
  :config
  ;; text-mode otherwise adds ispell-completion-at-point to
  ;; completion-at-point-functions, and each call spawns look/grep over a
  ;; word list; company hits it on every keystroke.
  (setopt text-mode-ispell-word-completion nil))

(use-package ispell
  :defer t
  :config
  (setenv "DICTIONARY" "en_GB")
  (setopt ispell-program-name "hunspell"
          ispell-local-dictionary "en_GB"
          ispell-local-dictionary-alist
          '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
            ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8))))

(when (bound-and-true-p enable-langtool)
  (use-package flymake-languagetool
    :hook ((latex-mode      . flymake-languagetool-load)
           (org-mode        . flymake-languagetool-load)
           (markdown-mode   . flymake-languagetool-load))
    :init
    (setopt flymake-languagetool-server-jar nil ;; not an actual path
            flymake-languagetool-url "http:localhost:8081"
            flymake-languagetool-language "en-GB")))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . az-prose-editing)
         (markdown-mode . visual-line-mode)
         (markdown-mode . (lambda () (setq-local yas-indent-line 'fixed))))
  :bind (:map markdown-mode-map ("C-c i" . insert-file-name-as-wikilink))
  :init
  (setopt markdown-command "multimarkdown"
          markdown-enable-wiki-links t
          markdown-wiki-link-alias-first t
          markdown-hide-urls t
          markdown-fontify-code-blocks-natively t
          markdown-wiki-link-search-type '(project)
          markdown-unordered-list-item-prefix "    - "
          markdown-italic-underscore t
          markdown-link-space-sub-char " ")
  :config
  (defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*fInsert file name: \nP")
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                    filename)) "]]"))))


(use-package olivetti
  :defer t
  :init
  (setopt olivetti-body-width 120
          ;; Olivetti turns on visual-line-mode by default. Modes that want
          ;; soft wrapping (markdown, mail) turn it on themselves.
          olivetti-mode-on-hook nil))

(use-package typst-ts-mode
  :hook (typst-ts-mode . eglot-ensure))

(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/nextcloud/99_archive/0000/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))

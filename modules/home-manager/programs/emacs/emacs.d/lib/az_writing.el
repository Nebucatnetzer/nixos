;; -*- lexical-binding: t; -*-
(defun az-lang-tool ()
  "Load flymake-languagetool and start flymake."
  (interactive)
  (require 'flymake-languagetool)
  (flymake-languagetool-maybe-load)
  (flymake-mode 1))

(use-package emacs
  :config
  ;; ispell settings
  (setenv "DICTIONARY" "en_GB")
  ;; ispell settings
  (setopt ispell-program-name "hunspell"
          ispell-local-dictionary "en_GB"
          ispell-local-dictionary-alist
          '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
            ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8))))

(when (boundp 'enable-langtool)
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
  (add-hook 'markdown-mode-hook '(lambda ()
                                   (set (make-local-variable
                                         'yas-indent-line) 'fixed)))
  :config
  (add-hook 'markdown-mode-hook
            (defun az-markdown-editing ()
              "My settings for message composition."
              (setopt whitespace-line-column 500)
              (turn-off-auto-fill)))
  (defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*fInsert file name: \nP")
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                    filename)) "]]")))

  (define-key markdown-mode-map (kbd "C-c i") 'insert-file-name-as-wikilink))

(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :init
  (setopt olivetti-body-width 120))

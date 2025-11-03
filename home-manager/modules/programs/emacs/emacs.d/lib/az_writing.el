;; -*- lexical-binding: t; -*-
(defun az-lang-tool ()
  "Load flymake-languagetool and start flymake."
  (interactive)
  (require 'flymake-languagetool)
  (flymake-languagetool-maybe-load)
  (flymake-mode 1))

(when (boundp 'enable-langtool)
  (use-package flymake-languagetool
    :hook ((latex-mode      . flymake-languagetool-load)
           (org-mode        . flymake-languagetool-load)
           (markdown-mode   . flymake-languagetool-load))
    :init
    (setq flymake-languagetool-server-jar "/etc/profiles/per-user/andreas/share/languagetool-server.jar" ;; not an actual path
          flymake-languagetool-language "en-GB")))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-enable-wiki-links t
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
              (setq whitespace-line-column 500)
              (turn-off-auto-fill)))
  (defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*fInsert file name: \nP")
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                    filename)) "]]")))

  (define-key markdown-mode-map (kbd "C-c i") 'insert-file-name-as-wikilink))

(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :init
  (setq olivetti-body-width 120))

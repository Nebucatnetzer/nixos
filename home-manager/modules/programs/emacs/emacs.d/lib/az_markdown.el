;; -*- lexical-binding: t; -*-
;; add markdown-mode to edit markdown files
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
              (turn-off-auto-fill)
              (flyspell-mode)))
  (defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*fInsert file name: \nP")
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                    filename)) "]]")))

  (define-key markdown-mode-map (kbd "C-c i") 'insert-file-name-as-wikilink))

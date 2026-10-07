;; -*- lexical-binding: t; -*-
;; https://github.com/minad/vertico
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-previous-group)
              ("C-k" . vertico-next-group)
              )
  :config
  ;; https://github.com/minad/consult/discussions/892#discussioncomment-14755154
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories
                 '(buffer (vertico-sort-function . nil))))
  (add-hook 'vertico-mode-hook #'vertico-multiform-mode)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

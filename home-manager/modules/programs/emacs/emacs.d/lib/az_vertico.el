;; -*- lexical-binding: t; -*-
;; https://github.com/minad/vertico
(use-package vertico
  :bind (:map vertico-map
              ("M-[" . vertico-previous-group)
              ("M-]" . vertico-next-group)
              )
  :init
  (setq completion-styles '(basic partial-completion substring))
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

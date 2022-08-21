(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-alh")

;; keymap for dired
(global-set-key (kbd "C-c d") 'dired-jump)

(bind-keys :map dired-mode-map ("q" . az-kill-dired-buffers))

;;a function to kill all dired buffers
(defun az-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(use-package dired-hide-dotfiles
  :ensure t
  :init
  (defun my-dired-mode-hook ()
    "My `dired' mode hook."
    ;; To hide dot-files by default
    (dired-hide-dotfiles-mode)

    ;; To toggle hiding

    (add-hook 'dired-mode-hook #'my-dired-mode-hook))
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

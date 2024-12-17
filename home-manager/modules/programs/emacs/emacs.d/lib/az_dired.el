;; -*- lexical-binding: t; -*-
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-ahl --group-directories-first")

;; keymap for dired
(global-set-key (kbd "C-x d") 'dired-jump)

(defun az-kill-dired-buffers ()
  "Kill all buffers in Dired mode."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (eq major-mode 'dired-mode))
      (kill-buffer buffer))))

(setq dired-auto-revert-buffer t)

(use-package emacs
  :init
  (with-eval-after-load 'locate
    (define-key locate-mode-map (kbd "SPC") 'god-execute-with-current-bindings))

  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "q" 'az-kill-dired-buffers)
    (evil-define-key 'normal dired-mode-map (kbd "SPC") 'god-execute-with-current-bindings))
  )

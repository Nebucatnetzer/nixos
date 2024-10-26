;; -*- lexical-binding: t; -*-
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-ahl --group-directories-first")

;; keymap for dired
(global-set-key (kbd "C-x d") 'dired-jump)

;; It's currently not working but would be handy to have.
;;(bind-keys :map dired-mode-map ("q" . az-kill-dired-buffers))

;;a function to kill all dired buffers
(defun az-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(setq dired-auto-revert-buffer t)

(use-package emacs
  :init
  (with-eval-after-load 'locate
    (define-key locate-mode-map (kbd "SPC") nil))
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "SPC") nil))
  ;;(with-eval-after-load 'evil
  ;; (define-key evil-normal-state-map (kbd "SPC") nil)
  ););)

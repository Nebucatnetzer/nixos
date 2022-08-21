(use-package treemacs
  :ensure t
  :bind ("<f12>" . treemacs)
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)))

(when (boundp 'enable-langtool)
  (use-package langtool
    :ensure t))

;; Amx, an alternative interface for M-x in Emacs
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :ensure t
  :config
  (amx-mode t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package gnu-elpa-keyring-update
  :ensure t)

;; enable magit a great git porcelain.
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("<f10>" . magit-status))

(use-package bug-hunter
  :defer t
  :ensure t)

(use-package direnv
  :config
  (direnv-mode))

;; My details

(setq user-full-name "Andreas Zweili")
(setq user-mail-address "andreas@zweili.ch")

;; Spaces instead of TABs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; enable hippie expand on M-Space
(global-set-key "\M- " 'hippie-expand)

;; file encodings
(prefer-coding-system 'utf-8-unix)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; initial buffers should use text-mode
(setq-default major-mode 'text-mode)

                                        ; Calender should start on Monday
(setq calendar-week-start-day 1)

;; insert only one space after a period
(setq sentence-end-double-space nil)

                                        ; Matches parentheses and such in every mode
(show-paren-mode 1)

;; pair parentheses
(electric-pair-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq ad-redefinition-action 'accept)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-auto-revert-mode t)

(setq-default fill-column 79)

(setq column-number-mode 1)

(when (boundp 'enable-emojis)
  (when (is-linux-p)
    (set-fontset-font t nil "Symbola" nil 'prepend)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))
(defun generic-setup ()
  ;; turn off `linum-mode' when there are more than 5000 lines
  (if (buffer-too-big-p) (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook 'generic-setup)
(add-hook 'text-mode-hook 'generic-setup)

(setq history-delete-duplicates t)

(xterm-mouse-mode 1)

(add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)))

(setq tramp-default-method "ssh")

(when (boundp 'enable-auctex)
  (defun LaTeX-collapse-table ()
    (interactive)
    (save-excursion
      (LaTeX-mark-environment)
      (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
        (replace-match " \\1"))))

  (defun LaTeX-align-environment (arg)
    (interactive "P")
    (if arg
        (LaTeX-collapse-table)
      (save-excursion
        (LaTeX-mark-environment)
        (align (region-beginning) (region-end))))))

(when (boundp 'enable-auctex)
  (defcustom LaTeX-inhibited-auto-fill-environments
    '("tabular" "tikzpicture") "For which LaTeX environments not to run auto-fill.")

  (defun LaTeX-limited-auto-fill ()
    (let ((environment (LaTeX-current-environment)))
      (when (not (member environment LaTeX-inhibited-auto-fill-environments))
        (do-auto-fill)))))

(when (boundp 'enable-auctex)
  (global-set-key (kbd "C-c f") 'LaTeX-align-environment)
  (setq auto-fill-function 'LaTeX-limited-auto-fill))

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

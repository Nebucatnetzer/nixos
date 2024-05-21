;; -*- lexical-binding: t; -*-
;; https://github.com/minad/consult
;; Example configuration for Consult
(use-package consult
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-x b" . consult-buffer)
         ("C-c j" . consult-git-grep))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")
  )

(use-package consult-projectile
  :bind
  (("C-x p" . consult-projectile)
   ("C-c k" . az-consult-git-grep-filetype)
   ("C-s" . consult-line)
   ("C-c g". consult-projectile)))

(defun az-consult-git-grep-filetype (query)
  (interactive "s#: ")
  (let* ((filetype (file-name-extension (buffer-file-name)))
         (output-buffer-name "*git-grep*")
         (default-directory (vc-root-dir))
         (cmd (format "git grep --line-number --full-name %s -- '*.%s'" query filetype)))
    (message "filetype: %s" filetype)
    (message "cmd: %s" cmd)
    (with-output-to-temp-buffer output-buffer-name
      (shell-command cmd output-buffer-name)))
  )

;; -*- lexical-binding: t; -*-
;; https://github.com/minad/consult
;; Example configuration for Consult
(use-package consult
  :demand t
  :init
  (with-eval-after-load 'consult
    (when-let* ((src (locate-library "consult-flymake.el")))
      (load src nil nil t)))   ; NOSUFFIX=t → load the .el, not the .elc

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-x C-b" . consult-buffer)
         ("C-c f" . consult-flymake)
         ("C-c j" . consult-ripgrep)                ;; orig. previous-matching-history-element
         ("C-c k" . az-consult-ripgrep-filetype)
         ("C-s" . consult-line))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (consult-customize
   consult-flymake
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.5 any))
  )

;; One list of project buffers (b), project files (f) and known projects (p).
(use-package consult-project-extra
  :bind (("C-x C-p" . consult-project-extra-find)
         ("C-c g" . consult-project-extra-find)))

(defun az-consult-ripgrep-filetype ()
  "Search the project with ripgrep, only in files with the current extension."
  (interactive)
  (let* ((extension (and buffer-file-name
                         (file-name-extension buffer-file-name)))
         (consult-ripgrep-args (if extension
                                   (concat consult-ripgrep-args
                                           " --glob=*." extension)
                                 consult-ripgrep-args)))
    (consult-ripgrep)))

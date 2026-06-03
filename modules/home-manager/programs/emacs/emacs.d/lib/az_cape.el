;; -*- lexical-binding: t; -*-

(defun az-cape-setup ()
  (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  (add-hook 'completion-at-point-functions #'cape-dict nil t)
  (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)
  (add-hook 'completion-at-point-functions #'cape-file nil t))

;; https://github.com/minad/cape
(use-package cape
  :custom                                                                                                                                                          │
  (cape-dict-file (getenv "EMACS_DICT_WORDS"))
  :hook
  ((prog-mode text-mode) . az-cape-setup))

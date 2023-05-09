(use-package persp-mode
  :hook (after-init . persp-mode)
  :init
  (setq persp-set-ido-hooks t
        persp-keymap-prefix (kbd "C-x x"))
  :config
  (persp-def-auto-persp "mail"
                        :parameters '((dont-save-to-file . t))
                        :buffer-name "mu4e*")
  (with-eval-after-load "persp-mode"
    (with-eval-after-load "ivy"
      (add-hook 'ivy-ignore-buffers
                #'(lambda (b)
                    (when persp-mode
                      (let ((persp (get-current-persp)))
                        (if persp
                            (not (persp-contain-buffer-p b persp))
                          nil)))))

      (setq ivy-sort-functions-alist
            (append ivy-sort-functions-alist
                    '((persp-kill-buffer   . nil)
                      (persp-remove-buffer . nil)
                      (persp-add-buffer    . nil)
                      (persp-switch        . nil)
                      (persp-window-switch . nil)
                      (persp-frame-switch  . nil)))))))

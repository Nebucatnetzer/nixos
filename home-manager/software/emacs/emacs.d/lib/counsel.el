(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-q") #'counsel-org-tag))
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  ;; (global-set-key (kbd "C-s") 'swiper-isearch)
  (evil-define-key 'normal 'global "/" 'swiper-isearch)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key ivy-minibuffer-map (kbd "S-SPC") (lambda () (interactive) (insert " ")))
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

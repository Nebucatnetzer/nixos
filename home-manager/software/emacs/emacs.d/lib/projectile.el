(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '(("~/.nixos" . 1)
                                         "~/git_repos/projects/"
                                         "~/git_repos/work/"))
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; -*- lexical-binding: t; -*-
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '(("~/.nixos" . 1)
                                         "~/git_repos/projects/"
                                         "~/git_repos/work/")
        projectile-completion-system 'ivy
        projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
        projectile-switch-project-action #'projectile-dired)
  (global-set-key (kbd "C-c g") 'projectile-find-file)
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)))

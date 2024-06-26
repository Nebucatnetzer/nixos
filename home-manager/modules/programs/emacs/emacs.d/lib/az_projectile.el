;; -*- lexical-binding: t; -*-
;; used because I can mark arbitrary directories as projects
;; Haven't found an alternative to the projectile-project-search-path yet
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '(("~/.nixos" . 1)
                                         "~/git_repos/projects/"
                                         "~/git_repos/work/")
        projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
        projectile-completion-system 'default
        projectile-switch-project-action #'projectile-dired)
  )

;; basic settings
(load-file "~/.emacs.d/variables.el")
(load-file (config-path "detect_os.el"))
(load-file (config-path "evil.el"))
(load-file (config-path "packages.el"))
(load-file (config-path "theme.el"))
(load-file (config-path "ui.el"))

;; note taking
(load-file (config-path "deft.el"))

;; org-mode
(load-file (config-path "org_insert.el"))

(load-file (config-path "loader.el"))

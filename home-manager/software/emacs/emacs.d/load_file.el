;; basic settings
(load-file "~/.emacs.d/variables.el")
(load-file (config-path "detect_os.el"))
(load-file (config-path "evil.el"))
(load-file (config-path "packages.el"))
(load-file (config-path "theme.el"))
(load-file (config-path "ui.el"))

;; keybindings
(load-file (config-path "hydra.el"))

;; note taking
(load-file (config-path "deft.el"))
(load-file (config-path "pdf_tools.el"))

;; org-mode
(load-file (config-path "org_insert.el"))

;; various
(load-file (config-path "counsel.el"))
(load-file (config-path "helpful.el"))
(load-file (config-path "Yasnippet.el"))


(load-file (config-path "loader.el"))

;; basic settings
(load-file "~/.emacs.d/variables.el")
(load-file (config-path "detect_os.el"))
(load-file (config-path "evil.el"))
(load-file (config-path "packages.el"))
(load-file (config-path "theme.el"))
(load-file (config-path "ui.el"))

;; keybindings
(load-file (config-path "hydra.el"))
(load-file (config-path "keybindings.el"))

;; note taking and writing
(load-file (config-path "deft.el"))
(load-file (config-path "latex.el"))
(load-file (config-path "markdown.el"))
(load-file (config-path "org_mode.el"))
(load-file (config-path "pdf_tools.el"))
(load-file (config-path "spellcheck.el"))

;; org-mode
(load-file (config-path "org_insert.el"))

;; programming
(load-file (config-path "company.el"))
(load-file (config-path "format_all.el"))
(load-file (config-path "powershell.el"))
(load-file (config-path "python.el"))
(load-file (config-path "webmode.el"))
(load-file (config-path "yaml.el"))

;; various
(load-file (config-path "counsel.el"))
(load-file (config-path "eyebrowse.el"))
(load-file (config-path "manuals.el"))
(load-file (config-path "mu4e.el"))
(load-file (config-path "window_split.el"))
(load-file (config-path "yasnippet.el"))


(load-file (config-path "loader.el"))

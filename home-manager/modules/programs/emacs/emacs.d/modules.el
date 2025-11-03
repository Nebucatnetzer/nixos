;; -*- lexical-binding: t; -*-
;; basic settings
(load-file (modules-path "az_general.el"))

;; keybindings and navigation
(load-file (modules-path "az_ace_window.el"))
(load-file (modules-path "az_avy.el"))
(load-file (modules-path "az_evil.el"))
(load-file (modules-path "az_god_mode.el"))
(load-file (modules-path "az_hydra.el"))

;; looks
(load-file (modules-path "az_theme.el"))
(load-file (modules-path "az_ui.el"))

;; completion
(load-file (modules-path "az_cape.el"))
(load-file (modules-path "az_consult.el"))
(load-file (modules-path "az_corfu.el"))
(load-file (modules-path "az_embark.el"))
(load-file (modules-path "az_marginalia.el"))
(load-file (modules-path "az_orderless.el"))
(load-file (modules-path "az_vertico.el"))

;; note taking and writing
(load-file (modules-path "az_denote.el"))
(load-file (modules-path "az_hyperbole.el"))
(load-file (modules-path "az_langtool.el"))
(load-file (modules-path "az_markdown.el"))
(load-file (modules-path "az_olivetti.el"))
(load-file (modules-path "az_org.el"))
(load-file (modules-path "az_org_insert.el"))
(load-file (modules-path "az_pdf_tools.el"))

;; programming
(load-file (modules-path "az_flymake_pylint.el"))
(load-file (modules-path "az_programming.el"))

;; various third party
(load-file (modules-path "az_manuals.el"))
(load-file (modules-path "az_mu4e.el"))
(load-file (modules-path "az_org_social_el.el"))
(load-file (modules-path "az_perspective.el"))
(load-file (modules-path "az_treemacs.el"))
(load-file (modules-path "az_ultra_scroll.el"))
(load-file (modules-path "az_vterm.el"))
(load-file (modules-path "az_vundo.el"))
(load-file (modules-path "az_yasnippet.el"))

;; -*- lexical-binding: t; -*-
(load-file (modules-path "az_paths.el"))

;; basic settings
(load-file (modules-path "az_lib.el"))
(load-file (modules-path "az_commands.el"))
(load-file (modules-path "az_core_settings.el"))

;; keybindings and navigation
(load-file (modules-path "az_evil.el"))
(load-file (modules-path "az_god_mode.el"))
(load-file (modules-path "az_hydra.el"))
(load-file (modules-path "az_navigation.el"))

;; looks
(load-file (modules-path "az_ui.el"))

;; completion
(load-file (modules-path "az_cape.el"))
(load-file (modules-path "az_consult.el"))
(load-file (modules-path "az_corfu.el"))
(load-file (modules-path "az_marginalia.el"))
(load-file (modules-path "az_orderless.el"))
(load-file (modules-path "az_vertico.el"))

;; note taking and writing
(load-file (modules-path "az_denote.el"))
(load-file (modules-path "az_org_core.el"))
(load-file (modules-path "az_org_agenda.el"))
(load-file (modules-path "az_org_export.el"))
(load-file (modules-path "az_org_insert.el"))
(load-file (modules-path "az_pdf_tools.el"))
(load-file (modules-path "az_writing.el"))

;; programming
(load-file (modules-path "az_flymake_pylint.el"))
(load-file (modules-path "az_programming.el"))

;; various third party
(load-file (modules-path "az_manuals.el"))
(load-file (modules-path "az_mu4e.el"))
(load-file (modules-path "az_perspective.el"))
(load-file (modules-path "az_treemacs.el"))
(load-file (modules-path "az_ghostel.el"))
(load-file (modules-path "az_vundo.el"))
(load-file (modules-path "az_yasnippet.el"))

;; -*- lexical-binding: t; -*-
(load-file (az-modules-path "az_paths.el"))

;; basic settings
(load-file (az-modules-path "az_lib.el"))
(load-file (az-modules-path "az_commands.el"))
(load-file (az-modules-path "az_core_settings.el"))

;; keybindings and navigation
(load-file (az-modules-path "az_evil.el"))
(load-file (az-modules-path "az_god_mode.el"))
(load-file (az-modules-path "az_navigation.el"))

;; looks
(load-file (az-modules-path "az_ui.el"))

;; completion
(load-file (az-modules-path "az_company.el"))
(load-file (az-modules-path "az_consult.el"))
(load-file (az-modules-path "az_marginalia.el"))
(load-file (az-modules-path "az_minuet.el"))
(load-file (az-modules-path "az_orderless.el"))
(load-file (az-modules-path "az_vertico.el"))

;; note taking and writing
(load-file (az-modules-path "az_denote.el"))
(load-file (az-modules-path "az_org_core.el"))
(load-file (az-modules-path "az_pdf_tools.el"))
(load-file (az-modules-path "az_writing.el"))

;; programming
(load-file (az-modules-path "az_programming.el"))

;; various third party
(load-file (az-modules-path "az_elfeed.el"))
(load-file (az-modules-path "az_manuals.el"))
(load-file (az-modules-path "az_mu4e.el"))
(load-file (az-modules-path "az_perspective.el"))
(load-file (az-modules-path "az_treemacs.el"))
(load-file (az-modules-path "az_ghostel.el"))
(load-file (az-modules-path "az_vundo.el"))
(load-file (az-modules-path "az_yasnippet.el"))

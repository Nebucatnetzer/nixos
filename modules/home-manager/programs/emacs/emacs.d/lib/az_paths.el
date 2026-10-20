;; -*- lexical-binding: t; -*-
;; The alias must come before the defvar, so an old setq of az-nextcloud-dir
;; in variables.el still sets az-data-dir.
(define-obsolete-variable-alias 'az-nextcloud-dir 'az-data-dir "2026-10")
(defvar az-data-dir "~/data/")

(defvar az-auto-uploads-dir "~/Downloads/inbox/photos/")
(defvar az-org-archive-dir (concat az-data-dir "99_archive/"))
(defvar az-org-app-dir (concat az-data-dir "98_applications/org/"))
(defvar az-org-bibliography-file (concat az-org-app-dir "bibliography.bib"))
(defvar az-org-html-setup-file (concat az-org-app-dir "export/html_theme/setup/theme-readtheorg-local.setup"))
(defvar az-org-inbox-dir (concat az-data-dir "01_inbox/"))
(defvar az-org-inbox-file (concat az-org-inbox-dir "00_inbox.org"))
(defvar az-org-latex-style-file (concat az-org-app-dir "export/latex/style.tex"))
(defvar az-org-projects-dir (concat az-data-dir "02_projects/"))
(defvar az-org-gitlab-file (concat az-org-projects-dir "00_gitlab.org"))
(defvar az-org-templates-dir (concat az-org-app-dir "templates/"))
(defvar az-org-work-file (concat az-org-projects-dir "00_work.org"))

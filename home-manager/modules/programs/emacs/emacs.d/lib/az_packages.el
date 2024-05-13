;; -*- lexical-binding: t; -*-
;; MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

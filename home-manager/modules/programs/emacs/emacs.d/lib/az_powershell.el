;; -*- lexical-binding: t; -*-
;; enable powershell-mode
(use-package powershell
  :mode
  (("\\.ps1\\'" . powershell-mode)
   ("\\.psm1\\'" . powershell-mode)))

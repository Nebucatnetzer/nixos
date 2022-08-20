;; enable powershell-mode
(use-package powershell
  :ensure t
  :mode
  (("\\.ps1\\'" . powershell-mode)
   ("\\.psm1\\'" . powershell-mode)))

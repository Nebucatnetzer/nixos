(use-package yaml-mode
  :defer t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode))
  :interpreter ("yml" . yml-mode))

(use-package ansible
  :defer t
  :mode
  (("\\.yml\\'" . ansible-mode)
   ("\\.yaml\\'" . ansible-mode))
  :init
  (define-derived-mode ansible-mode yaml-mode "Ansible"
    "Major mode which is YAML-mode + ansible minor mode."
    (ansible)))

;; -*- lexical-binding: t; -*-
;; web-mode for general web development
(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))
  (add-to-list 'auto-mode-alist '("\\.phpi$" . my/php-setup)))

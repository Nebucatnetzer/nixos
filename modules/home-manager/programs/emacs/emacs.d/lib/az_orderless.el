;; -*- lexical-binding: t; -*-
;; https://github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

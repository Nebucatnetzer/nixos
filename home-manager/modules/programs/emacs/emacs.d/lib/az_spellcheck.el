;; -*- lexical-binding: t; -*-
;; ispell settings
(setenv "DICTIONARY" "en_GB")
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_GB")
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
        ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8)))

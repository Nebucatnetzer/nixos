;; -*- lexical-binding: t; -*-
(defun az-lang-tool ()
  "Load flymake-languagetool and start flymake."
  (interactive)
  (require 'flymake-languagetool)
  (flymake-languagetool-maybe-load)
  (flymake-mode 1))

(when (boundp 'enable-langtool)
  (use-package flymake-languagetool
    :hook ((latex-mode      . flymake-languagetool-load)
           (org-mode        . flymake-languagetool-load)
           (markdown-mode   . flymake-languagetool-load))
    :init
    (setq flymake-languagetool-server-jar "/etc/profiles/per-user/andreas/share/languagetool-server.jar" ;; not an actual path
          flymake-languagetool-language "en-GB")))

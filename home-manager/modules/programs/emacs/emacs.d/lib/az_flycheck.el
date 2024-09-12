;; -*- lexical-binding: t; -*-

(use-package flymake
  :hook
  ((prog-mode . flymake-mode)
   (flymake-mode-hook . flymake-flycheck-auto)))

(defhydra hydra-flymake (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^
  ^────────^──────────^──────^────
  _q_ quit            _<_ previous
                      _>_ next
  ^^                  ^^
  "
  ("q" nil)
  ("<" flymake-goto-prev-error :color pink)
  (">" flymake-goto-next-error :color pink))

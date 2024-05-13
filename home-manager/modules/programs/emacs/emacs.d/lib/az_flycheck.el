;; -*- lexical-binding: t; -*-
;;; flycheck --- my config for flycheck
;;; Commentary:
;;; https://www.reddit.com/r/emacs/comments/931la6/tip_how_to_adopt_flycheck_as_your_new_best_friend/
;;; Code:

(use-package flycheck
  :hook
  ((python-mode . flycheck-mode)
   (yaml-mode . flycheck-mode)))

(defhydra hydra-flycheck (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

;; -*- lexical-binding: t; -*-
(use-package hydra)

(defhydra hydra-buffer (:color red :columns 3)
  "
                Buffers :
  "
  ("n" next-buffer "next")
  ("b" ivy-switch-buffer "switch")
  ("B" ibuffer "ibuffer")
  ("p" previous-buffer "prev")
  ("C-b" buffer-menu "buffer menu")
  ("N" evil-buffer-new "new")
  ("d" kill-this-buffer "delete")
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete")
  ("s" save-buffer "save"))

(defhydra hydra-window-operations (:color red :columns 3)
  "
                    Windows :
"
  ("h" windmove-left "left")
  ("l" windmove-right "right")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("w" (lambda () (interactive) (other-window 1)) "Unkown")
  ("h" split-window-vertically "horizontal split")
  ("v" split-window-horizontally "vertical split")
  ("o" delete-other-windows "Delete all other")
  ("X" delete-window "Delete this")
  ("=" balance-windows "Balance")
  ("K" kill-this-buffer "Kill Buffer")
  ("D" kill-all-dired-buffers "Kill all dired")
  ("S" toggle-window-split "Toggle Split"))

(defhydra hydra-ediff (:color blue :columns 3)
  "
                    Ediff :
"
  ("b" ediff-buffers "buffers")
  ("B" ediff-buffers3 "buffers 3-way")
  ("f" ediff-files "files")
  ("F" ediff-files3 "files 3-way")
  ("c" ediff-current-file "current file")
  ("r" ediff-revision "revision")
  ("l" ediff-regions-linewise "linewise")
  ("w" ediff-regions-wordwise "wordwise"))

(defhydra hydra-yasnippet (:color blue :columns 3)
  "
                    Yasnippet:
"
  ("d" yas-load-directory "directory")
  ("e" yas-activate-extra-mode "extra")
  ("i" yas-insert-snippet "insert")
  ("f" yas-visit-snippet-file "file")
  ("n" yas-new-snippet "new")
  ("t" yas-tryout-snippet "tryout")
  ("l" yas-describe-tables "list")
  ("a" yas-reload-all "reload"))

(defhydra hydra-apropos (:color blue :columns 3)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-customize-apropos (:color blue :columns 3)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))

(define-key Info-mode-map (kbd "?") #'hydra-info/body)
(defhydra hydra-info (:color blue
                             :hint nil)
  "
    Info-mode:

    ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
    ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
    ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
    ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
    regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

    _1_ .. _9_ Pick first .. ninth item in the node's menu.

    "
  ("]"   Info-forward-node)
  ("["   Info-backward-node)
  ("n"   Info-next)
  ("p"   Info-prev)
  ("s"   Info-search)
  ("S"   Info-search-case-sensitively)

  ("l"   Info-history-back)
  ("r"   Info-history-forward)
  ("H"   Info-history)
  ("t"   Info-top-node)
  ("<"   Info-top-node)
  (">"   Info-final-node)

  ("u"   Info-up)
  ("^"   Info-up)
  ("m"   Info-menu)
  ("g"   Info-goto-node)
  ("b"   beginning-of-buffer)
  ("e"   end-of-buffer)

  ("f"   Info-follow-reference)
  ("i"   Info-index)
  (","   Info-index-next)
  ("I"   Info-virtual-index)

  ("T"   Info-toc)
  ("d"   Info-directory)
  ("c"   Info-copy-current-node-name)
  ("C"   clone-buffer)
  ("a"   info-apropos)

  ("1"   Info-nth-menu-item)
  ("2"   Info-nth-menu-item)
  ("3"   Info-nth-menu-item)
  ("4"   Info-nth-menu-item)
  ("5"   Info-nth-menu-item)
  ("6"   Info-nth-menu-item)
  ("7"   Info-nth-menu-item)
  ("8"   Info-nth-menu-item)
  ("9"   Info-nth-menu-item)

  ("?"   Info-summary "Info summary")
  ("h"   Info-help "Info help"))

(defhydra hydra-spellchecking (:color blue :columns 3)
  "
    Spell Checking
    "
  ("g" languagetool-check "Check grammar")
  ("q" languagetool-check-done "Finish grammar check")
  ("l" languagetool-switch-default-language "Switch grammar language")
  ("m" languagetool-show-message-at-point "unkown")
  ("b" languagetool-correct-buffer "Correct grammar in buffer")
  ("s" ispell "Correct Spelling")
  ("d" ispell-change-dictionary "Change dictionary"))

                                        ;(define-key dired-mode-map (kbd "'") #'hydra-dired/body)
                                        ;https://github.com/abo-abo/hydra/wiki/Dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

                                        ;https://github.com/abo-abo/hydra/wiki/PDF-Tools
(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤  [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

(use-package general
  :config
  (general-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    "b" '(hydra-buffer/body t :which-key "Buffer")
    "w" '(hydra-window-operations/body t :which-key "Windows")
    "f" '(hydra-flycheck/body t :which-key "Flycheck")
    "s" '(hydra-spellchecking/body t :which-key "Spell Checking")
    "e" '(hydra-ediff/body t :which-key "Diffing")
    "i" '(hydra-yasnippet/body t :which-key "Yasnippets")
    "p" '(hydra-pdftools/body t :which-key "PDF Tools")
    "a" '(hydra-apropos/body t :which-key "Apropos Commands")))

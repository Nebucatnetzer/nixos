(load-file "~/.emacs.d/variables.el")

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun is-windows-p ()
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)
      (eq system-type 'cygwin)))

(defun is-bsd-p ()
  (eq system-type 'gnu/kfreebsd))

;; MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package use-package-ensure-system-package
  :ensure t)

;; evil-mode allows to use vim keybindings
(use-package evil
    :ensure t
    :init
    (setq evil-undo-system 'undo-redo)
    :config
    ;; Start these modes in emacs mode
    (add-to-list 'evil-emacs-state-modes 'deft-mode)
    (add-to-list 'evil-emacs-state-modes 'neotree-mode)
    (add-to-list 'evil-emacs-state-modes 'helpful-mode)
    (setq global-evil-search-highlight-persist t)

    ;; Start these modes in insert mode
    (evil-set-initial-state 'mu4e-compose-mode 'insert)

    ;; Add vim keybindings to the bookmark menu
    (evil-add-hjkl-bindings bookmark-bmenu-mode-map 'emacs
        ;; (kbd "/")       'evil-search-forward
        (kbd "n")       'evil-search-next
        (kbd "N")       'evil-search-previous
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

    ;; Add vim keybindings to the ibuffer
    (evil-add-hjkl-bindings ibuffer-mode-map 'emacs
        ;; (kbd "/")       'evil-search-forward
        (kbd "n")       'evil-search-next
        (kbd "N")       'evil-search-previous
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

;; Add vim keybindings to the package mode
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs
        ;; (kbd "/")       'evil-search-forward
        (kbd "n")       'evil-search-next
        (kbd "N")       'evil-search-previous
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

    (evil-add-hjkl-bindings helpful-mode-map 'emacs
        ;; (kbd "/")       'evil-search-forward
        (kbd "n")       'evil-search-next
        (kbd "N")       'evil-search-previous
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

    ;; vim keybindings for mu4e
    (evil-add-hjkl-bindings mu4e-headers-mode-map 'emacs
        (kbd "/")       'evil-search-forward
        (kbd "n")       'evil-search-next
        (kbd "N")       'evil-search-previous
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

    (evil-add-hjkl-bindings mu4e-view-mode-map 'emacs
        (kbd "C-d")     'evil-scroll-down
        (kbd "C-u")     'evil-scroll-up
        (kbd "C-w C-w") 'other-window)

    ;; evilificate calendar in org-mode
    (define-key org-read-date-minibuffer-local-map (kbd "M-h")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-l")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-k")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-j")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-H")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-L")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-K")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-J")
        (lambda ()
            (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

    (defun az-dired-up-directory ()
        "Take dired up one directory, but behave like dired-find-alternate-file"
        (interactive)
        (let ((old (current-buffer)))
            (dired-up-directory)
            (kill-buffer old)))

    ;; evil keybindings for dired
    (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "h" 'az-dired-up-directory)
    (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
    (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
    (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
    (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
    (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
    (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
    (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
    (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous))

    ;; evil keybindings for window movement
    (global-set-key (kbd "M-h") 'windmove-left)
    (global-set-key (kbd "M-l") 'windmove-right)
    (global-set-key (kbd "M-k") 'windmove-up)
    (global-set-key (kbd "M-j") 'windmove-down)

    (evil-mode 1))

;;; esc quits
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
        (when (get-buffer "*Completions*")
              (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(use-package evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode 1))

;; Color theme
;; disable background in terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(when (boundp 'enable-color-theme)
  ;; load solarized color theme
  (use-package solarized-theme
    :ensure t
    :config
    (setq solarized-use-variable-pitch nil)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-high-contrast-mode-line t)
    (set-face-inverse-video 'region nil)
    (load-theme 'solarized-light t)))

;; smooth scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 1
        scroll-conservatively 9999
        scroll-step 1))

(use-package deft
  :ensure t
  :bind ("<f5>" . deft)
  :commands (deft)
  :config
  (setq deft-extensions '("md")
        deft-default-extension "md"
        deft-markdown-mode-title-level 1
        deft-auto-save-interval 300.0
        deft-file-limit 50
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-recursive t)
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "_")
          (case-fn . downcase)))
  (setq deft-directory "~/nextcloud/10_documents/"))

(use-package zetteldeft
  :ensure t
  :after deft
  :config (zetteldeft-set-classic-keybindings)
  (setq zetteldeft-link-indicator "[["
        zetteldeft-link-suffix "]]")
  (setq zetteldeft-title-prefix "# "))

(use-package dash
  :defer t
  :ensure t)

(use-package swiper
  :defer t
  :ensure t)

(use-package s
  :defer t
  :ensure t)

(when (boundp 'enable-pdf-tools)
  (use-package pdf-tools
    :ensure t
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    ;; turn off cua so copy works
    (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
    ;; keyboard shortcuts
    (define-key pdf-view-mode-map (kbd "C-w C-w") 'other-window)
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-page-command)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-page-command)
    (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
    (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))

(use-package treemacs
  :ensure t
  :bind ("<f12>" . treemacs)
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)))

;; enable yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; enable yasnippet
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-global-mode 1))

;; which key is a package to show which keys can be pressed
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(when (boundp 'enable-langtool)
  (use-package langtool
    :ensure t))

(use-package hydra
  :ensure t)

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
  ("x" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)) "Unknown")
  ("z" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)) "Swap")
  ("=" balance-windows "Balance")
  ("K" kill-this-buffer "Kill Buffer")
  ("D" kill-all-dired-buffers "Kill all dired")
  ("a" ace-window "Ace")
  ("S" toggle-window-split "Toggle Split"))

                                        ;https://www.reddit.com/r/emacs/comments/931la6/tip_how_to_adopt_flycheck_as_your_new_best_friend/
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
  ("g" langtool-check "Check grammar")
  ("q" langtool-check-done "Finish grammar check")
  ("l" langtool-switch-default-language "Switch grammar language")
  ("m" langtool-show-message-at-point "unkown")
  ("b" langtool-correct-buffer "Correct grammar in buffer")
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

(defhydra hydra-elpy (:color red)
  "
  Navigate errors
  "
  ("n" next-error "next error")
  ("p" previous-error "previous error")
  ("d" (progn (call-interactively 'elpy-test-django-runner) (elpy-nav-errors/body)) "current test, Django runner" :color blue)
  ("t" (progn (call-interactively 'elpy-test-pytest-runner) (elpy-nav-errors/body)) "current test, pytest runner" :color blue)
  ("s" (progn
         (switch-to-buffer-other-window "*compilation*")
         (goto-char (point-max))) "switch to compilation buffer" :color blue)
  ("w" (venv-workon) "Workon venv…")
  ("q" nil "quit")
  ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue)
  )

(use-package general
  :ensure t
  :config
  (general-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    "b" '(hydra-buffer/body t :which-key "Buffer")
    "w" '(hydra-window-operations/body t :which-key "Windows")
    "r" '(hydra-restclient/body t :which-key "Restclient")
    "f" '(hydra-flycheck/body t :which-key "Flycheck")
    "s" '(hydra-spellchecking/body t :which-key "Spell Checking")
    "e" '(hydra-ediff/body t :which-key "Diffing")
    "i" '(hydra-yasnippet/body t :which-key "Yasnippets")
    "p" '(hydra-pdftools/body t :which-key "PDF Tools")
    "y" '(hydra-elpy/body t :which-key "Elpy")
    "a" '(hydra-apropos/body t :which-key "Apropos Commands")))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-c h f" . helpful-function)
         ("C-c h v" . helpful-variable)
         ("C-c h c" . helpful-command)
         ("C-c h m" . helpful-macro)
         ("<C-tab>" . backward-button)
         :map helpful-mode-map
         ("M-?" . helpful-at-point)
         :map emacs-lisp-mode-map
         ("M-?" . helpful-at-point)
         :map lisp-interaction-mode-map  ; Scratch buffer
         ("M-?" . helpful-at-point)))

(use-package amx
  :ensure t
  :config
  (amx-mode t))
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-q") #'counsel-org-tag))
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  ;; (global-set-key (kbd "C-s") 'swiper-isearch)
  (evil-define-key 'normal 'global "/" 'swiper-isearch)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key ivy-minibuffer-map (kbd "S-SPC") (lambda () (interactive) (insert " ")))
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c M-e"))
  (global-unset-key (kbd "C-c C-w"))
  (define-key evil-normal-state-map "gt" 'eyebrowse-next-window-config)
  (define-key evil-normal-state-map "gT" 'eyebrowse-prev-window-config)
  (define-key evil-normal-state-map "gr" 'eyebrowse-rename-window-config)
  (define-key evil-normal-state-map "gc" 'eyebrowse-close-window-config)
  :config
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-switch-back-and-forth t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode 1))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package gnu-elpa-keyring-update
  :ensure t)

;; web-mode for general web development
(use-package web-mode
  :ensure t
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

;; add markdown-mode to edit markdown files
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-enable-wiki-links t
        markdown-wiki-link-alias-first t
        markdown-hide-urls t
        markdown-fontify-code-blocks-natively t
        markdown-wiki-link-search-type '(project)
        markdown-link-space-sub-char " ")
  :config
  (defun insert-file-name-as-wikilink (filename &optional args)
    (interactive "*fInsert file name: \nP")
    (insert (concat "[[" (file-name-sans-extension (file-relative-name
                                                    filename)) "]]")))

  (define-key markdown-mode-map (kbd "C-c i") 'insert-file-name-as-wikilink))

;; enable powershell-mode
(use-package powershell
  :ensure t
  :mode
  (("\\.ps1\\'" . powershell-mode)
   ("\\.psm1\\'" . powershell-mode)))

(when (boundp 'enable-auctex)
  ;; auctex a greate plugin for latex writing
  (use-package latex
    :if (is-linux-p)
    :ensure auctex
    :mode ("\\.tex\\'" . latex-mode)
    :ensure-system-package
    (pdflatex . texlive-full)
    :config
    (setq-default TeX-master nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-electric-math (quote ("\\(" . "\\)")))
    (setq LaTeX-electric-left-right-brace t)
    (setq TeX-view-program-selection
          (quote
           (((output-dvi has-no-display-manager)
             "dvi2tty")
            ((output-dvi style-pstricks)
             "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "PDF Tools")
            (output-html "xdg-open"))))))

(use-package yaml-mode
  :defer t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode))
  :interpreter ("yml" . yml-mode)
  :ensure t)

(use-package python-mode
  :ensure t
  :defer t
  :config
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq eldoc-idle-delay 1)
  (add-hook 'python-mode-hook (lambda () (highlight-indentation-mode -1)))
  (elpy-enable))

(use-package company
  :ensure t
  :bind
  ("C-<tab>" . company-complete)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(when (boundp 'enable-auctex)
  (use-package company-auctex
    :if (is-linux-p)
    :ensure t
    :after auctex
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'company-auctex-init)))

(use-package company-web
  :ensure t
  :after web-mode
  :commands company-web-html
  :config
  (require 'company-web-html)

  ;; Tide completion support in web-mode with company-mode
  (defun my-web-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tide company-web-html company-yasnippet company-files)))

  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tide (before web-mode-set-up-ac-sources activate)
    "Set `tide-mode' based on current language before running company-tide."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tide-mode (tide-mode))
            (if tide-mode (tide-mode -1)))))))

(use-package company-restclient
  :ensure t
  :after (restclient company)
  :config (add-to-list 'company-backends 'company-restclient))

;; enable magit a great git porcelain.
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("<f10>" . magit-status))

;; change the colours of parenthesis the further out they are
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package bug-hunter
  :defer t
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character
        hightlight-indentation-mode nil
        highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "gray")
  (set-face-foreground 'highlight-indent-guides-character-face "gray")
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package format-all
  :ensure t
  :hook
  ((prog-mode . format-all-ensure-formatter)
   (yaml-mode . format-all-ensure-formatter)
   (prog-mode . format-all-mode))
  :preface
  (defun nebucatnetzer/format-code ()
    "format buffer."
    (interactive)
    (format-all-buffer))
  :config
  (global-set-key (kbd "C-c C-f") #'nebucatnetzer/format-code)
  (setq format-all-show-errors 'errors)
  (setq format-all-default-formatters
        '(("Assembly" asmfmt)
          ("ATS" atsfmt)
          ("Bazel" buildifier)
          ("BibTeX" emacs-bibtex)
          ("C" clang-format)
          ("C#" clang-format)
          ("C++" clang-format)
          ("Cabal Config" cabal-fmt)
          ("Clojure" zprint)
          ("CMake" cmake-format)
          ("Crystal" crystal)
          ("CSS" prettier)
          ("Cuda" clang-format)
          ("D" dfmt)
          ("Dart" dart-format)
          ("Dhall" dhall)
          ("Dockerfile" dockfmt)
          ("Elixir" mix-format)
          ("Elm" elm-format)
          ("Emacs Lisp" emacs-lisp)
          ("Erlang" efmt)
          ("F#" fantomas)
          ("Fish" fish-indent)
          ("Fortran Free Form" fprettify)
          ("GLSL" clang-format)
          ("Go" gofmt)
          ("GraphQL" prettier)
          ("Haskell" brittany)
          ("HTML" html-tidy)
          ("Java" clang-format)
          ("JavaScript" prettier)
          ("JSON" prettier)
          ("JSON5" prettier)
          ("Jsonnet" jsonnetfmt)
          ("JSX" prettier)
          ("Kotlin" ktlint)
          ("LaTeX" latexindent)
          ("Less" prettier)
          ("Literate Haskell" brittany)
          ("Lua" lua-fmt)
          ("Markdown" prettier)
          ("Nix" nixpkgs-fmt)
          ("Objective-C" clang-format)
          ("OCaml" ocp-indent)
          ("Perl" perltidy)
          ("PHP" prettier)
          ("Protocol Buffer" clang-format)
          ("PureScript" purty)
          ("Python" autopep8)
          ("R" styler)
          ("Reason" bsrefmt)
          ("ReScript" rescript)
          ("Ruby" rufo)
          ("Rust" rustfmt)
          ("Scala" scalafmt)
          ("SCSS" prettier)
          ("Shell" shfmt)
          ("Solidity" prettier)
          ("SQL" sqlformat)
          ("Svelte" prettier)
          ("Swift" swiftformat)
          ("Terraform" terraform-fmt)
          ("TOML" prettier)
          ("TSX" prettier)
          ("TypeScript" prettier)
          ("V" v-fmt)
          ("Verilog" istyle-verilog)
          ("Vue" prettier)
          ("XML" html-tidy)
          ("YAML" prettier)
          ("Zig" zig)
          ("_Angular" prettier)
          ("_Flow" prettier)
          ("_Gleam" gleam)
          ("_Ledger" ledger-mode)
          ("_Nginx" nginxfmt)
          ("_Snakemake" snakefmt)))
  )

(when (boundp 'enable-email)
  (use-package mu4e
    :if (is-linux-p)
    :config
    (require 'smtpmail)

    ;; use msmtp
    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    (setq sendmail-program "msmtp")

    (require 'mu4e)
    (require 'org-mu4e)

    (setq mu4e-completing-read-function (quote ivy-completing-read))
    (setq mail-user-agent 'mu4e-user-agent)

    (setq mu4e-drafts-folder "/personal/Drafts")
    (setq mu4e-sent-folder   "/personal/Sent")
    (setq mu4e-trash-folder  "/personal/Trash")
    (setq mu4e-refile-folder "/personal/Archive")

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")

    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text)
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

    (setq mu4e-headers-fields
          '((:date          .  10)    ;; alternatively, use :human-date
            (:flags         .   5)
            (:from          .  22)
            (:subject       .  nil))) ;; alternatively, use :thread-subject

    (setq mu4e-get-mail-command "offlineimap -qo"
          mu4e-update-interval 120
          mu4e-headers-auto-update t
          mu4e-compose-format-flowed t
          mu4e-index-update-in-background t
          mu4e-compose-dont-reply-to-self t
          mu4e-attachment-dir "~/nextcloud/10_documents/01_inbox"
          mu4e-compose-signature-auto-include nil)

    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

    (setq mu4e-maildir-shortcuts
          '(("/personal/INBOX" . ?i)
            ("/personal/Sent" . ?s)
            ("/personal/Trash" . ?t)
            ("/personal/Archive" . ?a)
            ("/personal/Drafts" . ?d)))

    ;; show images
    (setq mu4e-show-images t)

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setq mu4e-reply-to-address "andreas@zweili.ch")

    (setq message-kill-buffer-on-exit t)
    ;; Don't ask for a 'context' upon opening mu4e
    (setq mu4e-context-policy 'pick-first)
    ;; Don't ask to quit
    (setq mu4e-confirm-quit nil)

    ;; spell check
    (add-hook 'mu4e-compose-mode-hook
              (defun my-do-compose-stuff ()
                "My settings for message composition."
                (setq mu4e-compose-format-flowed t)
                (use-hard-newlines -1)
                (electric-indent-local-mode -1)
                (turn-off-auto-fill)
                (flyspell-mode)))))

;; disable menu and toolbar
(tool-bar-mode -1)
(menu-bar-mode -99)
(when (boundp 'enable-scroll-bar)
  (scroll-bar-mode -1))

                                        ; Proper line wrapping
(global-visual-line-mode 1)

(when (boundp 'disable-fringe)
                                        ; Disable fringe because I use visual-line-mode
  (set-fringe-mode '(0 . 0)))

                                        ; Disable splash screen
(setq inhibit-splash-screen t)

(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; disable or reconfigure prompts
(fset 'yes-or-no-p 'y-or-n-p) ;; remap yes or no to y or n

(setq confirm-nonexistent-file-or-buffer nil);; just create buffers don't ask
(setq ido-create-new-buffer 'always)

;; highlight bad whitespace
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face lines-tail tabs trailing))
  (set-face-attribute 'whitespace-line nil :foreground "#af005f")
  (global-whitespace-mode t))

(when (boundp 'enable-font)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(when (boundp 'enable-pdf-tools)
  ;; improve the resolution of doc-view
  (setq doc-view-resolution 200))

(toggle-frame-maximized)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                ;; "  "
                ;; mode-line-modes
                "   "
                mode-line-misc-info
                ;; battery-mode-line-string
                ;; mode-line-end-spaces
                ))

(setq display-line-numbers-type 'visual)
(add-hook 'prog-mode-hook (lambda ()
                            (when (version<= "26.0.50" emacs-version )
                              (display-line-numbers-mode))))

(setq inhibit-compacting-font-caches t)

(when (boundp 'enable-org-bullets)
  ;; Enable pretty bullets in org mode
  (use-package org-superstar
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1)))))

;; add a package to convert the agenda view to HTML
(use-package htmlize
  :ensure t
  :after org)

(when (is-linux-p)
;;; org-man.el - Support for links to manpages in Org

  (org-add-link-type "man" 'org-man-open)
  (add-hook 'org-store-link-functions 'org-man-store-link)

  (defcustom org-man-command 'man
    "The Emacs command to be used to display a man page."
    :group 'org-link
    :type '(choice (const man) (const woman)))

  (defun org-man-open (path)
    "Visit the manpage on PATH.
    PATH should be a topic that can be thrown at the man command."
    (funcall org-man-command path))

  (defun org-man-store-link ()
    "Store a link to a manpage."
    (when (memq major-mode '(Man-mode woman-mode))
      ;; This is a man page, we do make this link
      (let* ((page (org-man-get-page-name))
             (link (concat "man:" page))
             (description (format "Manpage for %s" page)))
        (org-store-link-props
         :type "man"
         :link link
         :description description))))

  (defun org-man-get-page-name ()
    "Extract the page name from the buffer name."
    ;; This works for both `Man-mode' and `woman-mode'.
    (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
        (match-string 1 (buffer-name))
      (error "Cannot create link to this man page")))

  (provide 'org-man)

;;; org-man.el ends here
  (require 'org-man))

(use-package org-ref
  :ensure t
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-default-citation-link "footcite")
  (setq reftex-default-bibliography '("~/nextcloud/03_documents/org/notes/_resources/references.bib"))
  (setq org-ref-bibliography-notes "~/nextcloud/03_documents/org/notes/bibliography_notes.org"
        org-ref-default-bibliography '("~/nextcloud/03_documents/org/notes/_resources/references.bib")
        org-ref-pdf-directory "~/nextcloud/03_documents/org/notes/_resources/"))

(when (boundp 'enable-ox-pandoc)
  (use-package ox-pandoc
    :ensure t
    :after org))

(use-package org
  :ensure t
  :pin gnu
  :config

  ;; enable org-mode keys
  (when (or (boundp 'enable-personal-agenda)
            (boundp 'enable-work-agenda))
    (define-key global-map "\C-ca" 'org-agenda))

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;; evil keybindings for the org-agenda
  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
    ;;(kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "c")       'org-capture
    (kbd "C-w C-w") 'other-window)

  ;; disable line split with M-RET
  (setq org-M-RET-may-split-line (quote ((default))))

  ;; enable the correct intdentation for source code blocks
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)

  ;; archive files to a monthly file
  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-archive-location
            (concat "~/nextcloud/10_documents/99_archive/2022/projects/"
                    (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))))
  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-archive-location
            (concat "~/nextcloud/03_documents/org/archive/work/"
                    (format-time-string "%Y-%m" (current-time)) "-%s::datetree/"))))

  ;; enable todo and checkbox depencies
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; quick access for todo states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w!)" "PROJECT(p)" "|" "DONE(d)")
          (sequence "|" "CANCELLED(c)")))

  (setq org-log-done 'time)

  (setq org-log-into-drawer t)

  ;; enable org-indent
  (setq org-startup-indented t)

  ;; capture templates
  (defun my/org-capture-read-file-name ()
    (concat (expand-file-name (read-file-name "PROMPT: " "~/nextcloud/12_tasks/")) ".org"))

  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-capture-templates
            (quote
             (("t" "Adds a Next entry" entry
               (file+headline "~/nextcloud/12_tasks/personal.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_personal_todo.txt")
               :empty-lines 1)
              ("n" "Add note" plain (file my/org-capture-read-file-name)
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_note.txt"))
              )))))

  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-capture-templates
            (quote
             (("j" "Journal Entry" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Clock")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_clock_note.txt")
               :empty-lines 1)
              ("c" "Phone call" entry (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Clock")
               "* %U PHONE %?" :clock-in t :clock-resume t)
              ("t" "Adds a Next entry" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_work_todo.txt")
               :clock-in t :clock-resume t)
              ("p" "Small Project" entry
               (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_work_small_project.txt"))
              ("m" "Meeting" entry (file+headline "~/nextcloud/03_documents/org/agenda/work/work.org" "Capture")
               "* %U MEETING: with %?\n" :clock-in t :clock-resume t :empty-lines 1)
              ("n" "Add note" plain (file my/org-capture-read-file-name)
               (file "~/nextcloud/10_documents/99_archive/0000/settings/templates/temp_note.txt"))
              )))))

  ;; org-columns format
  (setq org-columns-default-format
        "%40ITEM(Task) %8Effort(Estimated Effort){:} %8CLOCKSUM %10TAGS")

  ;; available effort times
  (setq org-global-properties
        (quote
         (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))

  ;; org-refile options
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (defun nebucatnetzer-org-files-list ()
    (delq nil
          (mapcar (lambda (buffer)
                    (buffer-file-name buffer))
                  (org-buffer-list 'files t))))

  (setq org-refile-targets '((nebucatnetzer-org-files-list :maxlevel . 6)))

  (setq org-src-fontify-natively t)

  (setq org-highlight-latex-and-related '(latex))

  (setq org-image-actual-width (quote (500)))
  (setq org-startup-with-inline-images t)

  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-clone-delete-id t)

  (setq org-blank-before-new-entry
        (quote ((heading . t)
                (plain-list-item . auto))))

  (setq org-footnote-section "Resources")

  (setq org-attach-directory "~/nextcloud/10_documents/99_archive/2022/resources/")

  (setq org-todo-keyword-faces
        `(("WAITING"   :foreground "#0087ff" :weight bold)
          ("TODO" :foreground "#d75f00" :weight bold)
          ("PROJECT"      :foreground "#626262" :weight bold)
          ("NEXT"      :foreground "#d70000" :weight bold)))


  (set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.00)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.00 :weight 'medium)
  (set-face-attribute 'org-agenda-calendar-event nil :weight 'medium)
  (set-face-attribute 'org-agenda-date nil :inherit 'default :height 1.00 :weight 'bold)
  (set-face-attribute 'org-agenda-date-today nil :slant 'normal :weight 'bold :height 1.00)
  (set-face-attribute 'org-done nil :foreground "#5f8700" :weight 'bold)
  (set-face-attribute 'org-link nil :foreground "#0087ff" :underline t)
  (set-face-attribute 'org-scheduled nil :foreground "#5f8700" :slant 'italic :weight 'normal)
  (set-face-attribute 'org-scheduled-previously nil :foreground "#d70000" :weight 'normal)
  (set-face-attribute 'org-scheduled-today nil :foreground "#5f8700" :slant 'italic :weight 'normal)
  (set-face-attribute 'org-todo nil :background "nil" :foreground "#d70000" :weight 'bold)
  (set-face-attribute 'org-upcoming-deadline nil :foreground "#d70000" :weight 'normal)
  (set-face-attribute 'org-warning nil :foreground "#d70000" :weight 'normal)

  (setq org-startup-shrink-all-tables t)

  ;; org-export formats
  ;;(setq org-export-backends (quote (beamer html latex md odt reveal)))

  (setq org-html-html5-fancy t
        org-html-doctype "html5")

  ;; disable the Todo keywords in the export
  (setq org-export-with-todo-keywords nil)

  ;; disable the tags in the export
  (setq org-export-with-tags nil)

  (setq org-latex-caption-above nil)

  (setq org-export-with-sub-superscripts nil)

  (setq org-export-with-smart-quotes t)

  (setq org-export-headline-levels 5)

  ;; options for beamer exports
  (setq org-beamer-frame-level 2)
  (setq org-beamer-outline-frame-options "")
  (setq org-beamer-outline-frame-title "Inhalt")
  (setq org-beamer-theme "metropolis")

  ;; options for latex exports
  (setq org-latex-classes
        (quote
         (("beamer" "\\documentclass{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("article" "\\documentclass{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("report" "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("book" "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
  (setq org-latex-default-packages-alist nil)
  (setq org-latex-listings 'listings)
  (setq org-latex-title-command "\\maketitle\\newpage")
  (setq org-latex-toc-command "\\tableofcontents
    \\newpage
    ")

  ;; Set the agenda separator to a space character.
  (setq org-agenda-block-separator " ")

  ;; a function to call the custom agenda view.
  (defun az/custom-agenda (&optional arg)
    (interactive "P")
    (org-agenda arg "A"))

  (global-set-key [f9] 'az/custom-agenda)

  ;; hide done tasks in the agenda
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)

  ;; Custom agenda command to list the stuck projects in the normal
  ;; agenda view.
  (setq org-stuck-projects '("/PROJECT" ("NEXT") nil ""))
  (setq org-agenda-custom-commands
        (quote (("A" "Custom Agenda"
                 ((agenda "" nil)
                  (stuck ""
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-sorting-strategy
                           '(category-up))))
                  (tags-todo "TODO=\"PROJECT\" "
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-sorting-strategy
                               '(category-up))))
                  nil))
                ;; Show all headings with the corresponding TODO state
                ("N" occur-tree "NEXT")
                ("O" occur-tree "TODO")
                ("W" occur-tree "WAITING"))))

  ;; don't show the warnings for deadlines if the item is scheduled
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

  ;; start the agenda on the current day and show the next 13 days
  (setq org-agenda-span 14
        org-agenda-start-on-weekday nil)
  (setq org-agenda-tags-column -80)
  (setq org-agenda-show-future-repeats (quote next))
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda todo-state-up priority-down category-up))))

  ;; dimm open tasks
  (setq org-agenda-dim-blocked-tasks t)

  ;; automatically refresh the agenda after adding a task
  (add-hook 'org-capture-after-finalize-hook 'nebucatnetzer:org-agenda-redo)

  (defun nebucatnetzer:org-agenda-redo ()
    (interactive)
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo t)
        (message "[org agenda] refreshed!"))))

  (setq org-clock-out-remove-zero-time-clocks t)

  (when (boundp 'enable-clocking)
    (defun start-heading-clock (id file)
      "Start clock programmatically for heading with ID in FILE."
      (require 'org-id)
      (if-let (marker (org-id-find-id-in-file id file t))
          (save-current-buffer
            (save-excursion
              (set-buffer (marker-buffer marker))
              (goto-char (marker-position marker))
              (org-clock-in)))
        (warn "Clock not started (Could not find ID '%s' in file '%s')" id file)))

    (defun start-main-clock ()
      "This functions always clocks in to the * Clock heading"
      (interactive)
      (start-heading-clock "e9f71012-4370-4dd2-af8e-9ae14d86508a" "~/nextcloud/03_documents/org/agenda/work/work.org"))

    (global-set-key (kbd "<f6>") 'start-main-clock))

  (org-clock-persistence-insinuate)

  (setq org-clock-out-when-done t)

  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  (global-set-key (kbd "<f7>") 'org-clock-in)
  (global-set-key (kbd "<f8>") 'org-clock-out)
  (global-set-key (kbd "C-x C-d") 'org-clock-mark-default-task)

  (setq org-duration-format (quote (("h") (special . 2))))

  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 4 :tcolumns 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; add image from conference phone upload                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; use case is taking a photo of a slide in a conference and uploading
  ;; it to google drive or dropbox or whatever to get it on your
  ;; computer. You then want to embed it in an org-mode document by
  ;; moving it to the same folder and renaming according to the current
  ;; section of the org file, avoiding name clashes

  ;; required libraries
  (require 'dash)
  (require 'swiper)
  (require 's)

  (global-set-key (kbd "C-c i") 'org-insert-image)
  ;; start directory
  (defvar bjm/conference-image-dir (expand-file-name "~/Downloads/"))

  (defun org-insert-image ()
    "Insert image from conference directory, rename and add link in current file.
    The file is taken from a start directory set by
    `bjm/conference-image-dir' and moved to the current directory, renamed
    and embedded at the point as an org-mode link. The user is presented
    with a list of files in the start directory, from which to select the
    file to move, sorted by most recent first."
    (interactive)
    (let (file-list target-dir file-list-sorted start-file start-file-full file-ext end-file end-file-base end-file-full file-number)
      ;; clean directories from list but keep times
      (setq file-list
            (-remove (lambda (x) (nth 1 x))
                     (directory-files-and-attributes bjm/conference-image-dir)))

      ;; get target directory
      (setq target-dir (concat (file-name-directory buffer-file-name) "_resources/"))
      (unless (file-exists-p target-dir)
        (make-directory target-dir))
      ;; sort list by most recent
      ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
      (setq file-list-sorted
            (mapcar #'car
                    (sort file-list
                          #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

      ;; use ivy to select start-file
      (setq start-file (ivy-read
                        (concat "Move selected file to " target-dir ":")
                        file-list-sorted
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input nil))

      ;; add full path to start file and end-file
      (setq start-file-full
            (expand-file-name start-file bjm/conference-image-dir))
      ;; generate target file name from current org section
      (setq file-ext
            (file-name-extension start-file t))

      ;; get section heading and clean it up
      (setq end-file-base (s-downcase (s-dashed-words (nth 4 (org-heading-components)))))
      ;; shorten to first 40 chars to avoid long file names
      (setq end-file-base (s-left 40 end-file-base))
      ;; number to append to ensure unique name
      (setq file-number 1)
      (setq end-file (concat
                      end-file-base
                      (format "-%s" file-number)
                      file-ext))

      ;; increment number at end of name if file exists
      (while (file-exists-p (concat "_resources/" end-file))
        ;; increment
        (setq file-number (+ file-number 1))
        (setq end-file (concat
                        end-file-base
                        (format "-%s" file-number)
                        file-ext)))

      ;; final file name including path
      (setq end-file-full
            (expand-file-name end-file target-dir))
      ;; rename file
      (rename-file start-file-full end-file-full)
      (message "moved %s to _resources/%s" start-file-full end-file)
      ;; insert link
      (insert (org-make-link-string (format "file:_resources/%s" end-file)))
      ;; display image
      (org-display-inline-images t t)))

  (load-library "find-lisp")
  (when (boundp 'enable-personal-agenda)
    (when (is-linux-p)
      (setq org-agenda-files
            (find-lisp-find-files "~/nextcloud/12_tasks" "\.org$"))))
  (when (boundp 'enable-work-agenda)
    (when (is-windows-p)
      (setq org-agenda-files
            (find-lisp-find-files "~/nextcloud/03_documents/org/agenda/work" "\.org$"))))

  (defun org-update-cookies-after-save()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (org-update-statistics-cookies "ALL")))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

;; keymap for my personal.org file
(when (boundp 'enable-personal-agenda)
  (when (is-linux-p)
    (global-set-key (kbd "C-c p")
                    (lambda () (interactive) (find-file "~/nextcloud/12_tasks/personal.org")))))
(when (boundp 'enable-work-agenda)
  (when (is-windows-p)
    (global-set-key (kbd "C-c p")
                    (lambda () (interactive) (find-file "~/nextcloud/03_documents/org/agenda/work/work.org")))))

;; My details

(setq user-full-name "Andreas Zweili")
(setq user-mail-address "andreas@zweili.ch")

;; a function to toggle the splits
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-map "4" 'toggle-window-split)

(defun nebucatnetzer:split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun nebucatnetzer:split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'nebucatnetzer:split-window-below-and-move-cursor)
(global-set-key (kbd "C-x 3") 'nebucatnetzer:split-window-right-and-move-cursor)

;; Spaces instead of TABs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; keymap for buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; kill THIS buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; copy the complete buffer to the clipboard
(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(global-set-key (kbd "C-S-c") 'copy-all)

;; keybinding for new frame
(global-set-key (kbd "C-x N") 'make-frame)

;; switch to frame
(global-set-key (kbd "C-x O") 'other-frame)

;; kill frame
(global-set-key (kbd "C-x K") 'delete-frame)

;; enable hippie expand on M-Space
(global-set-key "\M- " 'hippie-expand)

(defun switch-to-minibuffer () "Switch to minibuffer window."
       (interactive) (if (active-minibuffer-window)
                         (select-window
                          (active-minibuffer-window)) (error "Minibuffer is not active")))

(bind-key "M-m" 'switch-to-minibuffer)

;; file encodings
(prefer-coding-system 'utf-8-unix)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; hide temporary buffers
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-filter-by-name "^[^*]")))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . python-mode)
                 (mode . web-mode)
                 (mode . php-mode)
                 (mode . csharp-mode)
                 (mode . javascript-mode)
                 (mode . sql-mode)
                 (mode . powershell-mode)
                 (mode . nix-mode)
                 (mode . yaml-mode)
                 (mode . emacs-lisp-mode)))
               ;; etc
               ("Dired"
                (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; initial buffers should use text-mode
(setq-default major-mode 'text-mode)

                                        ; Calender should start on Monday
(setq calendar-week-start-day 1)

;; ispell settings
(setenv "DICTIONARY" "en_US")
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
        ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_CH") nil utf-8)))

;; insert only one space after a period
(setq sentence-end-double-space nil)

                                        ; Matches parentheses and such in every mode
(show-paren-mode 1)

;; pair parentheses
(electric-pair-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq ad-redefinition-action 'accept)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-auto-revert-mode t)

(setq-default fill-column 79)

(setq column-number-mode 1)

(when (boundp 'enable-emojis)
  (when (is-linux-p)
    (set-fontset-font t nil "Symbola" nil 'prepend)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq
         (current-buffer)
         (buffer-list))))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%d.%m.%Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-iso-date ()
  "Insert the current date in the ISO format."
  (interactive)
  (let ((format "%Y-%m-%d")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun insert-full-date ()
  "Insert the current date, write out the day and month name."
  (interactive)
  (let ((format "%A, %d. %B %Y")
        (system-time-locale "de_CH"))
    (insert (format-time-string format))))

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))
(defun generic-setup ()
  ;; turn off `linum-mode' when there are more than 5000 lines
  (if (buffer-too-big-p) (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook 'generic-setup)
(add-hook 'text-mode-hook 'generic-setup)

(setq history-delete-duplicates t)

(xterm-mouse-mode 1)

(add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq pcomplete-cycle-completions nil)))

(setq tramp-default-method "ssh")

(when (boundp 'enable-auctex)
  (defun LaTeX-collapse-table ()
    (interactive)
    (save-excursion
      (LaTeX-mark-environment)
      (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
        (replace-match " \\1"))))

  (defun LaTeX-align-environment (arg)
    (interactive "P")
    (if arg
        (LaTeX-collapse-table)
      (save-excursion
        (LaTeX-mark-environment)
        (align (region-beginning) (region-end))))))

(when (boundp 'enable-auctex)
  (defcustom LaTeX-inhibited-auto-fill-environments
    '("tabular" "tikzpicture") "For which LaTeX environments not to run auto-fill.")

  (defun LaTeX-limited-auto-fill ()
    (let ((environment (LaTeX-current-environment)))
      (when (not (member environment LaTeX-inhibited-auto-fill-environments))
        (do-auto-fill)))))

(when (boundp 'enable-auctex)
  (global-set-key (kbd "C-c f") 'LaTeX-align-environment)
  (setq auto-fill-function 'LaTeX-limited-auto-fill))

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-alh")

;; keymap for dired
(global-set-key (kbd "C-c d") 'dired-jump)

(bind-keys :map dired-mode-map ("q" . az-kill-dired-buffers))

;;a function to kill all dired buffers
(defun az-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(use-package dired-hide-dotfiles
  :ensure t
  :init
  (defun my-dired-mode-hook ()
    "My `dired' mode hook."
    ;; To hide dot-files by default
    (dired-hide-dotfiles-mode)

    ;; To toggle hiding

    (add-hook 'dired-mode-hook #'my-dired-mode-hook))
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

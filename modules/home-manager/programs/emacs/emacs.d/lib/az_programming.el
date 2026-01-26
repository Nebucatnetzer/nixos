;; -*- lexical-binding: t; -*-
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package format-all
  :hook
  ((prog-mode . format-all-ensure-formatter)
   (ansible-mode . format-all-ensure-formatter)
   (yaml-mode . format-all-ensure-formatter)
   (markdown-mode . format-all-ensure-formatter)
   (markdown-mode . format-all-mode)
   (prog-mode . format-all-mode))
  :preface
  (defun az-format-code ()
    "format buffer."
    (interactive)
    (format-all-buffer))
  (define-format-all-formatter docformatter
    (:executable "docformatter")
    (:install)
    (:languages "Python")
    (:features)
    (:format (format-all--buffer-easy executable "-")))
  :config
  (global-set-key (kbd "C-c C-f") #'az-format-code)
  (setopt format-all-show-errors 'errors
          format-all-formatters
          '(("Assembly" asmfmt)
            ("ATS" atsfmt)
            ("Bazel" buildifier)
            ("BibTeX" emacs-bibtex)
            ("C" clang-format)
            ("C#" clang-format)
            ("C++" clang-format)
            ("CMake" cmake-format)
            ("CSS" prettier)
            ("Cabal Config" cabal-fmt)
            ("Clojure" zprint)
            ("Crystal" crystal)
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
            ("HTML" html-tidy)
            ("Haskell" ormolu)
            ("JSON" prettier)
            ("JSON5" prettier)
            ("JSX" prettier)
            ("Java" (google-java-format))
            ("JavaScript" prettier)
            ("Jsonnet" jsonnetfmt)
            ("Kotlin" ktlint)
            ("LaTeX" latexindent)
            ("Less" prettier)
            ("Literate Haskell" brittany)
            ("Lua" lua-fmt)
            ("Markdown" prettier)
            ("Nix" nixfmt)
            ("OCaml" ocp-indent)
            ("Objective-C" clang-format)
            ("PHP" prettier)
            ("Perl" perltidy)
            ("Protocol Buffer" clang-format)
            ("PureScript" purty)
            ("Python" (isort) (docformatter "--black") (black))
            ("R" styler)
            ("ReScript" rescript)
            ("Reason" bsrefmt)
            ("Ruby" rufo)
            ("Rust" rustfmt)
            ("SCSS" prettier)
            ("SQL" sqlformat)
            ("Scala" scalafmt)
            ("Shell" (shfmt "-i" "4"))
            ("Solidity" prettier)
            ("Svelte" prettier)
            ("Swift" swiftformat)
            ("TOML" prettier)
            ("TSX" prettier)
            ("Terraform" terraform-fmt)
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
            ("_Snakemake" snakefmt))))

(use-package haskell-mode
  :hook
  (haskell-mode . eglot-ensure)
  (haskell-literate-mode . eglot-ensure))

(use-package flymake-ansible-lint
  :ensure t
  :commands flymake-ansible-lint-setup
  :hook ((ansible-mode . flymake-ansible-lint-setup)
         (ansible-mode . flymake-mode)))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package flymake
  :hook (sh-mode . flymake-mode))

(use-package eglot-mode
  :ensure nil
  :config
  (setopt eglot-autoshutdown t
          eldoc-echo-area-use-multiline-p nil
          gc-cons-threshold 100000000
          read-process-output-max (* 1024 1024))
  :bind
  (:map eglot-mode-map
        ("C-c C-r" . eglot-rename))
  :commands (eglot eglot-code-actions eglot-rename))

;; https://github.com/jdtsmith/eglot-booster
(unless (package-installed-p 'eglot-booster)
  (package-vc-install '(eglot-booster
                        :url "https://github.com/jdtsmith/eglot-booster"
                        )))
(use-package eglot-booster
  :ensure nil
  :after eglot
  :config
  (eglot-booster-mode))

;; optionally if you want to use debugger
(use-package dap-mode)

(use-package hurl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hurl\\'" . hurl-mode)))

(use-package jq-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((jq . t))))

(use-package magit
  :demand t
  :commands magit-status
  :bind
  ("<f10>" . magit-status)
  :hook (git-commit-setup . flyspell-mode)
  :config
  (setopt magit-diff-refine-hunk (quote all)
          magit-save-repository-buffers 'dontask))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . eglot-ensure))

(use-package powershell
  :mode
  (("\\.ps1\\'" . powershell-mode)
   ("\\.psm1\\'" . powershell-mode)))

;; used because I can mark arbitrary directories as projects
;; Haven't found an alternative to the projectile-project-search-path yet
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setopt projectile-project-search-path '(("~/.nixos" . 1)
                                           "~/git_repos/projects/"
                                           "~/git_repos/work/")
          projectile-completion-system 'default
          projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
          projectile-sort-order 'recently-active
          projectile-ignored-project-function 'file-remote-p
          projectile-switch-project-action #'projectile-dired)
  )

(use-package python-mode
  :config
  (setopt python-shell-interpreter "python3"
          flymake-pylint-executable "pylint")
  :hook ((python-ts-mode . eglot-ensure)
         (eglot-managed-mode . pylint-setup-flymake-backend)))

(use-package python-pytest
  :config
  (define-key python-ts-mode-map (kbd "C-c t" ) #'python-pytest-dispatch)
  )

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

;; https://github.com/federicotdn/verb
;; A very nice restclient working with org-mode
(use-package verb)

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

(use-package ansible
  :after yaml-ts-mode
  :config (add-hook 'yaml-ts-mode-hook '(lambda () (ansible-mode 1))))

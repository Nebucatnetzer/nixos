;; -*- lexical-binding: t; -*-
(when (boundp 'enable-org)
  (use-package org
    :config
    ;; Single owner of the registry: `org-babel-do-load-languages' replaces the
    ;; variable wholesale, so every enabled language must be listed here.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (jq . t)
       (python . t)
       (shell . t)))

    ;; `org-confirm-babel-evaluate' stays at its default t: every block asks
    ;; before it runs.

    ;; Reaching a nix shell from a block:
    ;;
    ;; 1. Preferred: put an .envrc next to the .org file. envrc advises
    ;;    `org-babel-eval', so blocks inherit the direnv environment. Note it
    ;;    keys off the org buffer's directory, not a block's :dir header.
    ;;
    ;; 2. One-off, via a nix-shell script header:
    ;;
    ;;    #+begin_src shell :shebang "#!/usr/bin/env nix-shell\n#! nix-shell -i bash -p jq"
    ;;    echo '{"a":42}' | jq -r .a
    ;;    #+end_src
    ;;
    ;; 3. One-off, inline:
    ;;
    ;;    #+begin_src shell
    ;;    nix-shell -p jq --run 'jq --version'
    ;;    #+end_src
    ;;
    ;; Both one-off forms pop *Org-Babel Error* on a cold run: nix writes
    ;; download progress to stderr and `org-babel-eval' treats any stderr as
    ;; failure, even when the block succeeded.

    ;; Without this a bare shell block returns its stdout whitespace-split into
    ;; a table, because `org-babel-result-cond' only sees ":results replace".
    (setopt org-babel-default-header-args:shell '((:results . "output")))))

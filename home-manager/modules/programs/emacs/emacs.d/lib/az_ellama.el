;; -*- lexical-binding: t; -*-
;; https://github.com/s-kostyaev/ellama

;; Key bindings:
;; "c c"    ellama-code-complete           Code complete
;; "c a"    ellama-code-add                Code add
;; "c e"    ellama-code-edit               Code edit
;; "c i"    ellama-code-improve            Code improve
;; "c r"    ellama-code-review             Code review
;; "c m"    ellama-generate-commit-message Generate commit message
;; "s s"    ellama-summarize               Summarize
;; "s w"    ellama-summarize-webpage       Summarize webpage
;; "s c"    ellama-summarize-killring      Summarize killring
;; "s l"    ellama-load-session            Session Load
;; "s r"    ellama-session-rename          Session rename
;; "s d"    ellama-session-remove          Session delete
;; "s a"    ellama-session-switch          Session activate
;; "i w"    ellama-improve-wording         Improve wording
;; "i g"    ellama-improve-grammar         Improve grammar and spelling
;; "i c"    ellama-improve-conciseness     Improve conciseness
;; "m l"    ellama-make-list               Make list
;; "m t"    ellama-make-table              Make table
;; "m f"    ellama-make-format             Make format
;; "a a"    ellama-ask-about               Ask about
;; "a i"    ellama-chat                    Chat (ask interactively)
;; "a l"    ellama-ask-line                Ask current line
;; "a s"    ellama-ask-selection           Ask selection
;; "t t"    ellama-translate               Text translate
;; "t b"    ellama-translate-buffer        Translate buffer
;; "t e"    ellama-chat-translation-enable Translation enable
;; "t d"    ellama-chat-translation-disable Translation disable
;; "t c"    ellama-complete                Text complete
;; "d w"    ellama-define-word             Define word
;; "x b"    ellama-context-add-buffer      Context add buffer
;; "x f"    ellama-context-add-file        Context add file
;; "x s"    ellama-context-add-selection   Context add selection
;; "x i"    ellama-context-add-info-node   Context add info node
;; "p s"    ellama-provider-select         Provider select

(use-package ellama
  :init
  ;; setup key bindings
  (setq llm-warn-on-nonfree nil)
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "German")

  (require 'auth-source)
  (setq openai-key (auth-source-pick-first-password :host "api.openai.com"))

  (require 'llm-openai)
  (setopt ellama-provider (make-llm-openai
                           :chat-model "gpt-4o-2024-08-06"
                           :embedding-model "gpt-4o-2024-08-06"
                           :key openai-key)))

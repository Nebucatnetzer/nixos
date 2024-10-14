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
  ;; (setopt ellama-keymap-prefix "C-c e")
  (global-set-key (kbd "C-c e") 'ellama-transient-main-menu)
  ;; language you want ellama to translate to
  (setopt ellama-language "German")

  (require 'auth-source)
  (setq openai-key (auth-source-pick-first-password :host "api.openai.com"))

  (require 'llm-openai)
  (setopt ellama-provider (make-llm-openai
                           :chat-model "gpt-4o-2024-08-06"
                           :embedding-model "gpt-4o-2024-08-06"
                           :key openai-key))
  (require 'transient)
  (transient-define-prefix ellama-transient-code-menu ()
    "Code Commands."
    [["Code Commands"
      ("c" "Complete" ellama-code-complete)
      ("a" "Add" ellama-code-add)
      ("e" "Edit" ellama-code-edit)
      ("i" "Improve" ellama-code-improve)
      ("r" "Review" ellama-code-review)
      ("m" "Generate Commit Message" ellama-generate-commit-message)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-summarize-menu ()
    "Summarize Commands."
    [["Summarize Commands"
      ("s" "Summarize" ellama-summarize)
      ("w" "Summarize Webpage" ellama-summarize-webpage)
      ("c" "Summarize Killring" ellama-summarize-killring)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-session-menu ()
    "Session Commands."
    [["Session Commands"
      ("l" "Load Session" ellama-load-session)
      ("r" "Rename Session" ellama-session-rename)
      ("d" "Remove Session" ellama-session-remove)
      ("a" "Activate Session" ellama-session-switch)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-improve-menu ()
    "Improve Commands."
    [["Improve Commands"
      ("w" "Improve Wording" ellama-improve-wording)
      ("g" "Improve Grammar" ellama-improve-grammar)
      ("c" "Improve Conciseness" ellama-improve-conciseness)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-make-menu ()
    "Make Commands."
    [["Make Commands"
      ("l" "Make List" ellama-make-list)
      ("t" "Make Table" ellama-make-table)
      ("f" "Make Format" ellama-make-format)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-ask-menu ()
    "Ask Commands."
    [["Ask Commands"
      ("i" "Chat" ellama-chat)
      ("l" "Ask Line" ellama-ask-line)
      ("s" "Ask Selection" ellama-ask-selection)
      ("a" "Ask About" ellama-ask-about)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-translate-menu ()
    "Translate Commands."
    [["Translate Commands"
      ("t" "Translate Text" ellama-translate)
      ("b" "Translate Buffer" ellama-translate-buffer)
      ("e" "Enable Translation" ellama-chat-translation-enable)
      ("d" "Disable Translation" ellama-chat-translation-disable)
      ("c" "Text Complete" ellama-complete)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-context-menu ()
    "Context Commands."
    [["Context Commands"
      ("b" "Add Buffer" ellama-context-add-buffer)
      ("f" "Add File" ellama-context-add-file)
      ("s" "Add Selection" ellama-context-add-selection)
      ("i" "Add Info Node" ellama-context-add-info-node)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-provider-menu ()
    "Provider Commands."
    [["Provider Commands"
      ("s" "Select Provider" ellama-provider-select)]
     ["Quit" ("q" "Quit" transient-quit-one)]])

  (transient-define-prefix ellama-transient-main-menu ()
    "Main Menu."
    [["Chat"
      ("a" "Ask" ellama-transient-ask-menu)
      ("c" "Code" ellama-transient-code-menu)]]
    [["Text"
      ("s" "Summarize" ellama-transient-summarize-menu)
      ("i" "Improve" ellama-transient-improve-menu)
      ("t" "Translate" ellama-transient-translate-menu)
      ("m" "Make" ellama-transient-make-menu)]]
    [["System"
      ("S" "Session" ellama-transient-session-menu)
      ("x" "Context" ellama-transient-context-menu)
      ("p" "Provider" ellama-transient-provider-menu)]]
    [["Quit" ("q" "Quit" transient-quit-one)]])
  )

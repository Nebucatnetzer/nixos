;; -*- lexical-binding: t; -*-
;; https://github.com/s-kostyaev/ellama

(use-package ellama
  :config
  (customize-set-variable 'ellama-auto-scroll t)
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
                           :chat-model "chatgpt-4o-latest"
                           :embedding-model "chatgpt-4o-latest"
                           :key openai-key))
  )

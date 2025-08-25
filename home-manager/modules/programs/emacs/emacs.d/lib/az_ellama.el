;; -*- lexical-binding: t; -*-
;; https://github.com/s-kostyaev/ellama

(use-package ellama
  :bind ("C-c e" . ellama)
  :init
  (setopt ellama-auto-scroll t)
  ;; setup key bindings
  (setq llm-warn-on-nonfree nil)

  ;; language you want ellama to translate to
  (setopt ellama-language "German")

  (require 'auth-source)
  (setq openai-key (auth-source-pick-first-password :host "api.openai.com"))

  (require 'llm-openai)
  (setopt ellama-provider (make-llm-openai
                           :chat-model "gpt-4.1"
                           :embedding-model "gpt-4.1"
                           :key openai-key))
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  )

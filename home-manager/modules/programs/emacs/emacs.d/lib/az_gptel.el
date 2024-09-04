;; -*- lexical-binding: t; -*-
;; https://github.com/karthink/gptel
(use-package gptel
  :config
  (setq
   gptel-model "chatgpt-4o-latest"
   gptel-backend
   (gptel-make-openai "openai-api"
                      :host "api.gptsapi.net"
                      :stream t
                      :models '("gpt-4" "gpt-4o" "chatgpt-4o-latest" "gpt-4o-mini"))))

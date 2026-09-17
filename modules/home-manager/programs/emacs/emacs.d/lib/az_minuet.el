;; -*- lexical-binding: t; -*-
;; https://github.com/milanglacier/minuet-ai.el
;; LLM code completion against Infomaniak, reusing the token and product id that pi
;; uses (see modules/home-manager/programs/pi/extensions/infomaniak-models.ts).

(defvar az-minuet-model "mistralai/Ministral-3-14B-Instruct-2512"
  "Infomaniak model id used for code completion.
The smallest model in the catalogue, so it answers fastest.  Alternatives, in
rising order of cost and latency: nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-FP8,
google/gemma-4-31B-it, moonshotai/Kimi-K2.6.  Run `pi --list-models' and `/rates'
on a host with pi for the current list and the effective per-token price.")

(defvar az-minuet-token-files
  (list (expand-file-name "agenix/infomaniakAiToken"
                          (or (getenv "XDG_RUNTIME_DIR") "/run/user/1000"))
        (expand-file-name "~/.config/minuet/infomaniak-token"))
  "Places to look for the Infomaniak API token, in order.
The first path is the agenix secret that pi also reads.  The second is a manual
fallback for machines without agenix, such as the WSL work setup.")

(defun az-minuet-token-file ()
  "Return the first readable file in `az-minuet-token-files', or nil."
  (seq-find #'file-readable-p az-minuet-token-files))

(defun az-minuet-api-key ()
  "Return the Infomaniak API token, or nil if no token file is readable.
Minuet calls this once per request, so the token is never stored in a variable."
  (let ((file (az-minuet-token-file)))
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(defun az-minuet-company-active-p ()
  "Return non-nil while the company popup has candidates.
Company completes symbols; minuet only fills in where company has nothing."
  (and (bound-and-true-p company-candidates) t))

;; The library check keeps the portable az-emacs build quiet, where minuet is not
;; installed but the token file may still exist.
(when (and (locate-library "minuet") (az-minuet-token-file))
  (use-package minuet
    :bind (("C-c m m" . minuet-auto-suggestion-mode)
           ("C-c m s" . minuet-show-suggestion)
           ("C-c m y" . minuet-complete-with-minibuffer)
           :map minuet-active-mode-map
           ("TAB" . minuet-accept-suggestion)
           ("<tab>" . minuet-accept-suggestion)
           ("M-n" . minuet-next-suggestion)
           ("M-p" . minuet-previous-suggestion)
           ("M-a" . minuet-accept-suggestion-line)
           ("M-w" . minuet-accept-suggestion-word)
           ("M-e" . minuet-dismiss-suggestion))
    :hook (prog-mode . minuet-auto-suggestion-mode)
    :config
    (setopt minuet-provider 'openai-compatible
            ;; Cost control against the CHF 20 monthly budget: one completion per
            ;; request, a quarter of the default context, and a slower throttle.
            minuet-n-completions 1
            minuet-context-window 4000
            minuet-request-timeout 5
            minuet-auto-suggestion-debounce-delay 0.6
            minuet-auto-suggestion-throttle-delay 3.0)
    ;; add-to-list, not setopt: the default already holds
    ;; minuet-evil-not-insert-state-p.
    (add-to-list 'minuet-auto-suggestion-block-predicates
                 #'az-minuet-company-active-p)
    (plist-put minuet-openai-compatible-options
               :end-point
               "https://api.infomaniak.com/2/ai/109278/openai/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key #'az-minuet-api-key)
    (plist-put minuet-openai-compatible-options :name "Infomaniak")
    (plist-put minuet-openai-compatible-options :model az-minuet-model)
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
    (minuet-set-optional-options minuet-openai-compatible-options :temperature 0.2)))

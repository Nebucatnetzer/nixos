(when (boundp 'enable-email)
  (use-package mu4e
    :if (is-linux-p)
    :ensure nil
    :config

    ;; Start mu4e-compose-mode in insert mode
    (evil-set-initial-state 'mu4e-compose-mode 'insert)

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
          ;; don't show threading by default:
          mu4e-headers-show-threads nil
          ;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
          mu4e-hide-index-messages t
          ;; Don't show related messages
          mu4e-headers-include-related nil
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

    ;; A function to create a persp for reading mail
    (defun open-mail ()
      "Create a mail perspective and open mu4e"
      (interactive)
      (persp-switch "mail")
      (mu4e))

    ;; spell check
    (add-hook 'mu4e-compose-mode-hook
              (defun az-do-compose-stuff ()
                "My settings for message composition."
                (setq mu4e-compose-format-flowed t)
                (use-hard-newlines -1)
                (turn-off-auto-fill)
                (flyspell-mode)))))

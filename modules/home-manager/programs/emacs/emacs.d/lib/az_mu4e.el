;; -*- lexical-binding: t; -*-
(when (bound-and-true-p enable-email)
  (use-package mu4e
    :if (is-linux-p)
    :bind
    ([f4] . open-mail)
    (:map mu4e-main-mode-map
          ("J" . mu4e-search-maildir))
    :config

    ;; Start mu4e-compose-mode in insert mode
    (evil-set-initial-state 'mu4e-compose-mode 'insert)
    (evil-define-key 'normal mu4e-view-mode-map (kbd "SPC") 'god-execute-with-current-bindings)

    ;; use msmtp
    (setopt message-send-mail-function 'message-send-mail-with-sendmail
            sendmail-program "msmtp")

    (setopt mail-user-agent 'mu4e-user-agent

            mu4e-drafts-folder "/personal/Drafts"
            mu4e-sent-folder   "/personal/Sent"
            mu4e-trash-folder  "/personal/Trash"
            mu4e-refile-folder "/personal/Archive"
            )

    (setopt mu4e-headers-fields
            '((:date          .  10)    ;; alternatively, use :human-date
              (:flags         .   5)
              (:from          .  22)
              (:subject       .  nil))) ;; alternatively, use :thread-subject

    (setopt message-dont-reply-to-names #'mu4e-personal-or-alternative-address-or-empty-p
            message-signature nil
            mu4e-get-mail-command "offlineimap -qo"
            mu4e-update-interval 120
            mu4e-headers-auto-update t
            mu4e-compose-format-flowed t
            mu4e-index-update-in-background t
            mu4e-attachment-dir az-org-inbox-dir
            ;; don't show threading by default:
            mu4e-search-threads nil
            ;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
            mu4e-hide-index-messages t
            ;; Don't show related messages
            mu4e-search-include-related nil)

    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

    (setopt mu4e-maildir-shortcuts
            '(("/personal/INBOX" . ?i)
              ("/personal/Sent" . ?s)
              ("/personal/Trash" . ?t)
              ("/personal/Archive" . ?a)
              ("/personal/Drafts" . ?d)))

    (setopt message-kill-buffer-on-exit t)
    ;; Don't ask for a 'context' upon opening mu4e
    (setopt mu4e-context-policy 'pick-first)
    ;; Don't ask to quit
    (setopt mu4e-confirm-quit nil)

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
                (use-hard-newlines -1)
                (visual-line-mode 1)
                (flyspell-mode)))))

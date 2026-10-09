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

    (require 'mu4e-contrib)
    (setopt mu4e-html2text-command 'mu4e-shr2text)
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

    (setopt mu4e-headers-fields
            '((:date          .  10)    ;; alternatively, use :human-date
              (:flags         .   5)
              (:from          .  22)
              (:subject       .  nil))) ;; alternatively, use :thread-subject

    (setopt mu4e-get-mail-command "offlineimap -qo"
            mu4e-update-interval 120
            mu4e-headers-auto-update t
            mu4e-compose-format-flowed t
            mu4e-index-update-in-background t
            mu4e-compose-dont-reply-to-self t
            mu4e-attachment-dir az-org-inbox-dir
            ;; don't show threading by default:
            mu4e-headers-show-threads nil
            ;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
            mu4e-hide-index-messages t
            ;; Don't show related messages
            mu4e-headers-include-related nil
            mu4e-compose-signature-auto-include nil)

    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

    (setopt mu4e-maildir-shortcuts
            '(("/personal/INBOX" . ?i)
              ("/personal/Sent" . ?s)
              ("/personal/Trash" . ?t)
              ("/personal/Archive" . ?a)
              ("/personal/Drafts" . ?d)))

    ;; show images
    (setopt mu4e-show-images t)

    ;; general emacs mail settings; used when composing e-mail
    ;; the non-mu4e-* stuff is inherited from emacs/message-mode
    (setopt mu4e-reply-to-address "andreas@zweili.ch")

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

(when (boundp 'enable-email)
  (use-package mu4e
    :if (is-linux-p)
    :config

    ;; Start mu4e-compose-mode in insert mode
    (evil-set-initial-state 'mu4e-compose-mode 'insert)

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

    ;; spell check
    (add-hook 'mu4e-compose-mode-hook
              (defun my-do-compose-stuff ()
                "My settings for message composition."
                (setq mu4e-compose-format-flowed t)
                (use-hard-newlines -1)
                (electric-indent-local-mode -1)
                (turn-off-auto-fill)
                (flyspell-mode)))))

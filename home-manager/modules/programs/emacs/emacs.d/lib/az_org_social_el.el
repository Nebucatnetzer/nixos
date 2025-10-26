(use-package org-social
  :config
  (evil-set-initial-state 'org-social-ui-mode 'emacs)
  (setopt org-social-file "~/git_repos/projects/org-posts/org-social/social.org"  ;; Path to your local file
          org-social-relay "https://org-social-relay.andros.dev/"  ;; Relay server
          org-social-my-public-url "https://zweili.ch/org-social/social.org"  ;; Your public URL
          ;; Hide Reply, Vote, and Profile buttons for a cleaner timeline view. Change to 't' to hide them. Keyboard shortcuts 'r', 'v', and 'P' still work
          org-social-hide-post-buttons nil

          ;; Set base URL for live post previews. When configured, a Share button will appear in post buttons
          ;; that opens the post preview in the system browser with URL-encoded post URL
          ;; Example: (setq org-social-live-preview-url "https://org-social-preview.andros.dev/?post=")
          org-social-live-preview-url "https://org-social-preview.andros.dev/?post="

          ;; Use only relay followers instead of local follow list
          org-social-only-relay-followers-p nil)

  ;; Optionally, configure global keybindings
  (keymap-global-set "C-c s t" #'org-social-timeline)
  (keymap-global-set "C-c s n" #'org-social-new-post)
  (keymap-global-set "C-c s o" #'org-social-open-file)
  (keymap-global-set "C-c s p" #'org-social-new-poll)
  (keymap-global-set "C-c s m" #'org-social-mention-user)
  )

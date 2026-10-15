;; -*- lexical-binding: t; -*-
(require 'dom)
(require 'shr)
(require 'url-cache)
(require 'url-queue)

(defvar az-elfeed-image-cache-directory (expand-file-name "~/.elfeed/images/")
  "Directory for prefetched feed images.
url.el deletes entries older than `url-cache-expire-time' from its own
cache, so the images live in a separate directory.")

(defvar az-elfeed-fever-host-url "https://andreas@rss.zweili.org"
  "FreshRSS host for elfeed-protocol. The user name must be part of the URL.")

(defvar az-elfeed-fever-api-url "https://rss.zweili.org/api/fever.php"
  "Fever API endpoint of FreshRSS.")

(defvar az-elfeed-keep-tags '(unread star)
  "Entries with one of these tags are kept. Every other entry is pruned on sync.
`star' is the default of `elfeed-protocol-fever-star-tag'.")

(defun az-elfeed--image-urls (html base-url)
  "Return the absolute URLs of the images shr shows for HTML.
BASE-URL resolves relative image URLs."
  (let ((dom (with-temp-buffer
               (insert html)
               (libxml-parse-html-region (point-min) (point-max) base-url)))
        (shr-base (shr-parse-base base-url))
        (urls nil))
    (dolist (image (dom-by-tag dom 'img))
      (let ((url (shr--preferred-image image))
            (size-attributes (list (dom-attr image 'width) (dom-attr image 'height))))
        ;; shr skips images with a width or height of 0 or 1 (tracking pixels).
        (unless (or (null url)
                    (member "0" size-attributes)
                    (member "1" size-attributes))
          (let ((absolute-url (shr-expand-url url)))
            (when (and (string-match-p "\\`https?:" absolute-url)
                       (not (member absolute-url urls)))
              (push absolute-url urls))))))
    (nreverse urls)))

(defun az-elfeed--image-cached-p (url)
  "Return non-nil if URL is in the image cache."
  (let ((url-cache-directory az-elfeed-image-cache-directory))
    (url-is-cached url)))

(defun az-elfeed--store-image (status url)
  "Write the current HTTP response buffer to the image cache under URL.
STATUS is the status plist from `url-queue-retrieve'."
  ;; Store under the requested URL, not the redirect target, because
  ;; shr looks up the URL from the feed.
  (unless (plist-get status :error)
    (let* ((url-cache-directory az-elfeed-image-cache-directory)
           (file (url-cache-create-filename url)))
      (when (url-cache-prepare file)
        (let ((coding-system-for-write 'binary))
          (write-region (point-min) (point-max) file nil 'silent)))))
  (kill-buffer (current-buffer)))

(defun az-elfeed--keep-entry-p (entry)
  "Return non-nil if ENTRY has one of `az-elfeed-keep-tags'."
  (let ((keep nil))
    (dolist (tag az-elfeed-keep-tags)
      (when (memq tag (elfeed-entry-tags entry))
        (setq keep t)))
    keep))

(defun az-elfeed--entry-image-urls (entry)
  "Return the image URLs in the HTML content of ENTRY."
  (let ((content (elfeed-deref (elfeed-entry-content entry)))
        (link (elfeed-entry-link entry)))
    (when (and content link (eq (elfeed-entry-content-type entry) 'html))
      (az-elfeed--image-urls content (elfeed-compute-base link)))))

(defun az-elfeed--prefetch-images (entry)
  "Queue downloads for the images of ENTRY that are not cached yet.
Skip entries that the next sync prunes."
  (when (az-elfeed--keep-entry-p entry)
    (dolist (url (az-elfeed--entry-image-urls entry))
      (unless (az-elfeed--image-cached-p url)
        (url-queue-retrieve url #'az-elfeed--store-image (list url) t t)))))

(defun az-elfeed--delete-cached-image (url)
  "Delete the cached image of URL if it exists."
  (let* ((url-cache-directory az-elfeed-image-cache-directory)
         (file (url-cache-create-filename url)))
    (when (file-exists-p file)
      (delete-file file))))

(defun az-elfeed--prune-read-entries ()
  "Delete the entries without `az-elfeed-keep-tags' and their cached images.
Images that a kept entry also uses stay in the cache."
  (let ((kept-entries nil)
        (pruned-entries nil))
    (elfeed-db-visit (entry _feed)
      (if (az-elfeed--keep-entry-p entry)
          (push entry kept-entries)
        (push entry pruned-entries)))
    (when pruned-entries
      (let ((kept-urls (make-hash-table :test #'equal)))
        (dolist (entry kept-entries)
          (dolist (url (az-elfeed--entry-image-urls entry))
            (puthash url t kept-urls)))
        (dolist (entry pruned-entries)
          (dolist (url (az-elfeed--entry-image-urls entry))
            (unless (gethash url kept-urls)
              (az-elfeed--delete-cached-image url))))
        (elfeed-db-delete pruned-entries)
        (elfeed-log 'info "Pruned %d read entries" (length pruned-entries))))))

(defun az-elfeed--insert-html-from-image-cache (insert-html &rest arguments)
  "Call INSERT-HTML with ARGUMENTS so that shr reads the image cache."
  (let ((url-cache-directory az-elfeed-image-cache-directory))
    (apply insert-html arguments)))

(defun az-elfeed-prefetch-all-images ()
  "Queue image downloads for every entry in the elfeed database."
  (interactive)
  (elfeed-db-visit (entry _feed)
    (az-elfeed--prefetch-images entry)))

(defun az-elfeed-prune-image-cache (days)
  "Delete cached feed images older than DAYS."
  (interactive "nDelete cached images older than how many days? ")
  (let ((url-cache-directory az-elfeed-image-cache-directory)
        (url-cache-expire-time (* days 24 60 60)))
    (url-cache-prune-cache)))

(defun az-open-feeds ()
  "Create a feeds perspective and open elfeed.
This does not sync. Use `az-elfeed-sync' for that."
  (interactive)
  (persp-switch "feeds")
  (elfeed))

(defun az-elfeed--open-unless-in-mode (&rest modes)
  "Open elfeed with `az-open-feeds' unless the buffer is in one of MODES."
  (unless (apply #'derived-mode-p modes)
    (az-open-feeds)))

(defun az-elfeed-show-random-entry ()
  "Show a random entry from the entries that match the current filter."
  (interactive)
  (az-elfeed--open-unless-in-mode 'elfeed-search-mode 'elfeed-show-mode)
  (with-current-buffer (elfeed-search-buffer)
    (unless elfeed-search-entries
      (user-error "No entries match the filter"))
    (elfeed-search-show-entry (seq-random-elt elfeed-search-entries))))

(defun az-elfeed-filter ()
  "Edit the search filter of elfeed live."
  (interactive)
  (az-elfeed--open-unless-in-mode 'elfeed-search-mode)
  (call-interactively #'elfeed-search-live-filter))

(defun az-elfeed--dispatch (search-function show-function &rest arguments)
  "Call SEARCH-FUNCTION or SHOW-FUNCTION with ARGUMENTS.
The search function acts on the entries at point or in the region.
The show function acts on the displayed entry."
  (cond
   ((derived-mode-p 'elfeed-search-mode) (apply search-function arguments))
   ((derived-mode-p 'elfeed-show-mode) (apply show-function arguments))
   (t (user-error "Not in an elfeed buffer"))))

(defun az-elfeed-mark-unread ()
  "Mark the selected or displayed entries as unread."
  (interactive)
  (az-elfeed--dispatch #'elfeed-search-tag #'elfeed-show-tag 'unread))

(defun az-elfeed-mark-read ()
  "Mark the selected or displayed entries as read.
The next sync prunes them unless they are starred."
  (interactive)
  (az-elfeed--dispatch #'elfeed-search-untag #'elfeed-show-untag 'unread))

(defun az-elfeed--show-toggle-tag (tag)
  "Toggle TAG on the displayed entry."
  (if (elfeed-tagged-p tag elfeed-show-entry)
      (elfeed-show-untag tag)
    (elfeed-show-tag tag)))

(defun az-elfeed-toggle-star ()
  "Toggle the star on the selected or displayed entries.
Starred entries are not pruned."
  (interactive)
  (az-elfeed--dispatch #'elfeed-search-toggle #'az-elfeed--show-toggle-tag 'star))

(defun az-elfeed--server-reachable-p ()
  "Return non-nil if the FreshRSS API answers."
  (eq 0 (call-process elfeed-curl-program-name nil nil nil
                      "--silent"
                      "--head"
                      "--output" "/dev/null"
                      "--connect-timeout" "5"
                      "--max-time" "10"
                      az-elfeed-fever-api-url)))

(defun az-elfeed--update-when-queue-empty ()
  "Run `elfeed-update' after all queued requests are finished."
  (if (> (elfeed-queue-count-total) 0)
      (run-at-time 0.5 nil #'az-elfeed--update-when-queue-empty)
    (elfeed-update)))

(defun az-elfeed-sync ()
  "Send the stored read and star changes to FreshRSS, then update the entries.
Stop if FreshRSS is not reachable, so that the stored changes are kept."
  (interactive)
  (require 'elfeed)
  (unless (az-elfeed--server-reachable-p)
    (user-error "FreshRSS is not reachable, nothing was synced"))
  (when (> (elfeed-queue-count-total) 0)
    (user-error "elfeed is still running requests, try again later"))
  ;; Send the changes first and wait. The unread list from FreshRSS
  ;; must already contain them, otherwise read entries come back as unread.
  (elfeed-protocol-fever-sync-pending-ids az-elfeed-fever-host-url)
  (az-elfeed--update-when-queue-empty))

(defun az-elfeed--save-db-soon (&rest _arguments)
  "Save the elfeed database after a short idle time.
This keeps read and star changes after a crash or a power loss."
  (elfeed-db--save-idle))

(defvar-keymap az-feeds-map
  :doc "Feed commands on C-l C-e."
  "C-e" #'az-open-feeds
  "C-u" #'az-elfeed-sync
  "C-r" #'az-elfeed-show-random-entry
  "C-S-u" #'az-elfeed-mark-unread
  "C-d" #'az-elfeed-mark-read
  "C-s" #'az-elfeed-toggle-star
  "C-f" #'az-elfeed-filter)

(use-package elfeed
  :if (bound-and-true-p enable-feeds)
  :defer t
  :init
  (keymap-set az-map "C-e" az-feeds-map)
  :config
  (require 'elfeed-protocol)
  (setopt elfeed-use-curl t)
  ;; setq, because the :type of elfeed-protocol-feeds rejects the plist form
  ;; that the package documents, and setopt then warns.
  (setq elfeed-protocol-feeds `((,(concat "fever+" az-elfeed-fever-host-url)
                                 :api-url ,az-elfeed-fever-api-url
                                 :use-authinfo t)))
  ;; Store read and star changes until `az-elfeed-sync', so they work offline.
  (setq elfeed-protocol-lazy-sync t)
  (add-hook 'elfeed-tag-hook #'az-elfeed--save-db-soon)
  (add-hook 'elfeed-untag-hook #'az-elfeed--save-db-soon)
  ;; The url-queue default of 5 s aborts large comic downloads.
  (setopt url-queue-timeout 30)
  ;; Required so that we can download fresh entries from FreshRSS
  (setopt elfeed-protocol-fever-update-unread-only t)
  (add-hook 'elfeed-new-entry-hook #'az-elfeed--prefetch-images)
  (add-hook 'elfeed-update-init-hook #'az-elfeed--prune-read-entries)
  (advice-add 'elfeed-insert-html :around #'az-elfeed--insert-html-from-image-cache)
  (elfeed-protocol-enable)

  ;; space is god-mode
  (evil-define-key 'normal elfeed-search-mode-map (kbd "SPC") 'god-execute-with-current-bindings)
  (evil-define-key 'normal elfeed-show-mode-map (kbd "SPC") 'god-execute-with-current-bindings)

  ;; open the article in the browser, like a link in org-mode
  (keymap-set elfeed-search-mode-map "C-c C-o" #'elfeed-search-browse-url)
  (keymap-set elfeed-show-mode-map "C-c C-o" #'elfeed-show-visit))

;; -*- lexical-binding: t; -*-
(when (boundp 'enable-clocking)

  (defvar az-gitlab-host ""
    "Hostname of the GitLab instance, e.g. \"gitlab.example.com\".
Set this in ~/.emacs.d/variables.el, which loads before this module.")

  (defvar az-gitlab-priority-labels
    '(("priority::high"   . "A")
      ("priority::normal" . "B")
      ("priority::low"    . "C"))
    "Maps a GitLab scoped label to an org priority cookie letter.
Labels outside this list are discarded.")

  (defvar az-gitlab-page-size 100
    "Issues requested per API page.")

  (defvar az-gitlab-max-pages 20
    "Runaway guard on pagination.")

  ;; --- pure helpers -------------------------------------------------------

  (defun az/gitlab--issue-priority (labels)
    "Return the org priority letter matching LABELS, or nil."
    (let ((letter nil))
      (dolist (entry az-gitlab-priority-labels)
        (when (and (not letter) (member (car entry) labels))
          (setq letter (cdr entry))))
      letter))

  (defun az/gitlab--issue-project (issue)
    "Return the \"group/project\" path of ISSUE."
    (let ((reference (alist-get 'full (alist-get 'references issue)))
          (web-url (alist-get 'web_url issue)))
      (cond
       (reference (car (split-string reference "#")))
       ;; Older GitLab omits `references'; recover it from the issue URL.
       ((and web-url
             (string-match "\\`https?://[^/]+/\\(.+\\)/-/issues/[0-9]+" web-url))
        (match-string 1 web-url))
       (t (error "Cannot determine the project of issue %S" issue)))))

  (defun az/gitlab--project-slug (project)
    "Return the last path segment of PROJECT."
    (car (last (split-string project "/"))))

  (defun az/gitlab--org-timestamp (iso8601)
    "Convert the UTC ISO8601 string to an inactive org timestamp in local time."
    (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time iso8601)))

  (defun az/gitlab--parse-issues (json)
    "Parse a GitLab issues JSON array into a list of plists."
    (let ((issues nil))
      (dolist (raw (json-parse-string json :object-type 'alist :array-type 'list))
        (push (list :url      (alist-get 'web_url raw)
                    :title    (alist-get 'title raw)
                    :project  (az/gitlab--issue-project raw)
                    :priority (az/gitlab--issue-priority (alist-get 'labels raw))
                    :created  (az/gitlab--org-timestamp (alist-get 'created_at raw)))
              issues))
      (nreverse issues)))

  (defun az/gitlab--heading-string (issue level)
    "Render ISSUE as an org heading of LEVEL stars."
    (let ((priority (plist-get issue :priority)))
      (concat (make-string level ?*) " TODO "
              (if priority (format "[#%s] " priority) "")
              (plist-get issue :title) "\n"
              ":PROPERTIES:\n"
              ":URL: " (plist-get issue :url) "\n"
              ":CREATED: " (plist-get issue :created) "\n"
              ":END:\n")))

  ;; --- api ---------------------------------------------------------------

  ;; `url-retrieve-synchronously' reads this dynamically.  Without the
  ;; declaration a byte-compiled build binds it lexically instead and the token
  ;; header is silently dropped, which surfaces only as an opaque HTTP 401.
  (defvar url-request-extra-headers)
  (defvar url-http-end-of-headers)

  (defun az/gitlab--base-url ()
    "Return the API base URL, erroring out when the host is unset."
    (when (string-empty-p az-gitlab-host)
      (user-error "Set `az-gitlab-host' in ~/.emacs.d/variables.el first"))
    (format "https://%s" az-gitlab-host))

  (defun az/gitlab--token ()
    "Return the GitLab private token for `az-gitlab-host' via auth-source.
Expects this line in ~/.authinfo.gpg:
  machine HOST login USER password TOKEN"
    (require 'auth-source)
    (let* ((entry (car (auth-source-search :host az-gitlab-host :max 1
                                           :require '(:secret))))
           (secret (plist-get entry :secret)))
      (unless secret
        (user-error "No auth-source entry for %s; add \"machine %s login USER password TOKEN\""
                    az-gitlab-host az-gitlab-host))
      (if (functionp secret) (funcall secret) secret)))

  (defun az/gitlab--get-page (page token)
    "Return the JSON body of one page of issues assigned to me."
    (require 'url)
    (let* ((url-request-extra-headers (list (cons "PRIVATE-TOKEN" token)))
           (target (format (concat "%s/api/v4/issues?scope=assigned_to_me"
                                   "&state=opened&per_page=%d&page=%d")
                           (az/gitlab--base-url) az-gitlab-page-size page))
           (buffer (url-retrieve-synchronously target t)))
      (unless buffer
        (user-error "No response from %s" az-gitlab-host))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (unless (re-search-forward "\\`HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (user-error "Malformed response from %s" az-gitlab-host))
            (let ((status (string-to-number (match-string 1))))
              (unless (< 199 status 300)
                (user-error "GitLab returned HTTP %d for page %d" status page)))
            (decode-coding-string
             (buffer-substring-no-properties url-http-end-of-headers (point-max))
             'utf-8))
        (kill-buffer buffer))))

  (defun az/gitlab-fetch-assigned-issues ()
    "Return every open issue assigned to me, across all visible projects."
    (let ((token (az/gitlab--token))
          (page 1)
          (issues nil)
          (exhausted nil))
      (while (and (not exhausted) (<= page az-gitlab-max-pages))
        (let ((batch (az/gitlab--parse-issues (az/gitlab--get-page page token))))
          (setq issues (append issues batch))
          (when (< (length batch) az-gitlab-page-size)
            (setq exhausted t))
          (setq page (1+ page))))
      (unless exhausted
        (warn "GitLab pagination hit the %d page cap; results may be incomplete"
              az-gitlab-max-pages))
      issues))

  ;; --- buffer ------------------------------------------------------------

  (defun az/gitlab--existing-urls ()
    "Map every URL property in the current buffer to a marker on its heading."
    (let ((urls (make-hash-table :test 'equal)))
      (org-map-entries
       (lambda ()
         (let ((url (org-entry-get (point) "URL")))
           (when url (puthash url (point-marker) urls)))))
      urls))

  (defun az/gitlab--project-heading (project)
    "Return the position of PROJECT's heading, appending it when absent."
    (let ((marker (org-find-exact-headline-in-buffer project (current-buffer))))
      (if marker
          (marker-position marker)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((start (point)))
          ;; CATEGORY is an org special property and inherits, so the issue
          ;; headings below show the slug in the agenda instead of the filename.
          (insert "* " project "\n"
                  ":PROPERTIES:\n"
                  ":CATEGORY: " (az/gitlab--project-slug project) "\n"
                  ":END:\n")
          start))))

  (defun az/gitlab--update-issue (issue)
    "Bring the heading at point in line with ISSUE, returning non-nil if changed.
GitLab is authoritative for the title and the priority.  A heading in a
done state is reopened, since ISSUE being fetched means it is open and
still assigned."
    (let* ((components (org-heading-components))
           (current-priority (nth 3 components))
           (wanted-priority (plist-get issue :priority))
           (changed nil))
      (unless (equal (nth 4 components) (plist-get issue :title))
        (org-edit-headline (plist-get issue :title))
        (setq changed t))
      (unless (equal (and current-priority (char-to-string current-priority))
                     wanted-priority)
        (if wanted-priority
            (org-priority (string-to-char wanted-priority))
          (org-priority 'remove))
        (setq changed t))
      (when (member (org-get-todo-state) org-done-keywords)
        (org-todo "TODO")
        (setq changed t))
      changed))

  (defun az/gitlab--insert-issue (issue)
    "Append ISSUE as a level 2 heading under its project heading."
    (goto-char (az/gitlab--project-heading (plist-get issue :project)))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert (az/gitlab--heading-string issue 2)))

  (defun az/gitlab--clocking-here-p ()
    "Non-nil when the org clock runs inside the entry at point."
    (and (org-clocking-p)
         (eq (marker-buffer org-clock-marker) (current-buffer))
         (>= (marker-position org-clock-marker)
             (save-excursion (org-back-to-heading t) (point)))
         (< (marker-position org-clock-marker)
            (save-excursion (org-end-of-subtree t t) (point)))))

  (defun az/gitlab--stale-markers (known-urls)
    "Return markers for open issue headings whose URL is absent from KNOWN-URLS."
    (let ((markers nil))
      (org-map-entries
       (lambda ()
         (let ((url (org-entry-get (point) "URL")))
           (when (and url
                      (not (gethash url known-urls))
                      (member (org-get-todo-state) org-not-done-keywords))
             (push (point-marker) markers)))))
      (nreverse markers)))

  (defun az/gitlab--close-stale (known-urls)
    "Mark DONE each open issue missing from KNOWN-URLS, returning the count."
    (let ((markers (az/gitlab--stale-markers known-urls))
          (closed 0))
      (dolist (marker markers)
        (goto-char (marker-position marker))
        ;; Read the clock first: `org-clock-out-when-done' makes `org-todo'
        ;; stop the clock, so afterwards there is nothing left to detect.
        (let ((was-clocked (az/gitlab--clocking-here-p)))
          (org-todo "DONE")
          (setq closed (1+ closed))
          (when was-clocked
            ;; `start-main-clock' is defined inside a deferred `use-package org
            ;; :config' block, which aborts on its first error, so it can be
            ;; missing even though this module loaded.  Never drop the clock
            ;; silently: the whole point here is not to lose tracked time.
            (if (fboundp 'start-main-clock)
                (start-main-clock)
              (warn "Closed a clocked GitLab issue but `start-main-clock' is undefined; clock lost"))))
        (set-marker marker nil))
      closed))

  (defun az/org-gitlab-sync ()
    "Sync GitLab issues assigned to me into `az-org-gitlab-file'.
Appends any issue not already present, marks DONE any open heading whose
issue is no longer assigned or open, and brings the title, priority and
reopened state of the remaining headings in line with GitLab.  Your own
tags, notes and clock history are left alone."
    (interactive)
    (require 'org)
    (let* ((issues (az/gitlab-fetch-assigned-issues))
           (fetched (make-hash-table :test 'equal))
           (added 0)
           (updated 0)
           closed)
      (dolist (issue issues)
        (puthash (plist-get issue :url) t fetched))
      (with-current-buffer (find-file-noselect az-org-gitlab-file)
        (when (zerop (buffer-size))
          (insert "#+title: GitLab\n\n"))
        (let ((existing (az/gitlab--existing-urls)))
          (setq closed (az/gitlab--close-stale fetched))
          (dolist (issue issues)
            (let ((marker (gethash (plist-get issue :url) existing)))
              (cond
               (marker
                (goto-char (marker-position marker))
                (when (az/gitlab--update-issue issue)
                  (setq updated (1+ updated))))
               (t
                (az/gitlab--insert-issue issue)
                (setq added (1+ added)))))))
        (save-buffer))
      (message "GitLab sync: %d new, %d updated, %d closed" added updated closed))))

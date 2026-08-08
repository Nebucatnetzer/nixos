;; -*- lexical-binding: t; -*-
;; Dated log entries per project, plus an aggregated view across all of them.
;;
;; A project's entries live in a `* Log' tree inside its own denote note, so
;; they archive together with the project.  Entries that have no project yet
;; go to the denote journal.  The chronological cross project view is built on
;; demand and never stored.
(when (boundp 'enable-notes)

  (require 'denote-journal)
  (require 'find-lisp)
  (require 'org-element)
  (require 'seq)
  (require 'subr-x)

  (defvar az-org-log-heading "Log"
    "Heading under which dated log entries are stored.")

  (defvar az-org-log-days 14
    "Number of days back to include in the aggregated log.")

  (defvar az-org-log-buffer "*Project Log*"
    "Name of the buffer displaying the aggregated log.")

  (defvar az-org-log-unassigned-label "(unassigned)"
    "Label shown for journal entries that belong to no project.")

;;; --- locating the files we log into -------------------------------------

  (defun az-org-log--project-files ()
    "Return the org files that represent projects.
Only files directly in `az-org-projects-dir' or one directory below it
count.  Anything deeper is attachment or archive material, such as the
old journal tree inside the reorganising-my-notes project."
    (seq-filter
     (lambda (file)
       (<= (length (split-string (file-relative-name file az-org-projects-dir) "/"))
           2))
     (find-lisp-find-files az-org-projects-dir "\\.org$")))

  (defun az-org-log--journal-directories ()
    "Return every year's journal directory under the archive.
Globbing all years rather than the current one keeps the aggregation
window intact across New Year.  Never glob the archive as a whole:
archived projects live there and carry their own Log trees."
    (file-expand-wildcards (expand-file-name "*/journal/" az-org-archive-dir)))

  (defun az-org-log--journal-files ()
    "Return every journal org file, across all years."
    (apply #'append
           (mapcar (lambda (dir) (directory-files dir t "\\.org\\'"))
                   (az-org-log--journal-directories))))

  (defun az-org-log--file-date (file)
    "Return the YYYYMMDD part of FILE's denote identifier, or nil."
    (let ((name (file-name-nondirectory file)))
      (when (string-match "\\`\\([0-9]\\{8\\}\\)T[0-9]\\{6\\}" name)
        (match-string 1 name))))

  (defun az-org-log--file-title (file)
    "Return the org title of FILE, or nil when it has none.
Only the head of the file is read: front matter is always at the top and
this runs once per project every time the picker or the panel opens."
    (with-temp-buffer
      (insert-file-contents file nil 0 2000)
      (delay-mode-hooks (org-mode))
      (org-get-title)))

  (defun az-org-log--display-name (file)
    "Return a readable name for FILE.
Prefer the org title, then the denote file name title, then the bare name."
    (or (az-org-log--file-title file)
        (if-let* ((title (denote-retrieve-filename-title file)))
            (replace-regexp-in-string "-" " " title)
          (file-name-base file))))

;;; --- the journal, as the destination for unassigned entries -------------

  (defun az-org-log--sync-journal-directory (&rest _)
    "Point `denote-journal-directory' at the current year.
`az_denote.el' computes this once at load time, so a long running daemon
would otherwise keep writing into last year's directory after New Year.
Takes and ignores arguments so it can be used as :before advice."
    (setq denote-journal-directory
          (expand-file-name (concat (format-time-string "%Y") "/journal/")
                            az-org-archive-dir)))

  (defun az-org-log--journal-file ()
    "Return the path to today's journal file, creating it if needed."
    (az-org-log--sync-journal-directory)
    (denote-journal-path-to-new-or-existing-entry))

;;; --- choosing a destination ---------------------------------------------

  (defun az-org-log--project-alist ()
    "Return an alist of (DISPLAY-NAME . PATH) for every project file."
    (mapcar (lambda (file)
              (cons (az-org-log--display-name file) file))
            (sort (az-org-log--project-files) #'string>)))

  (defun az-org-log--completion-table (candidates)
    "Return a completion table over CANDIDATES that keeps their order."
    (lambda (string predicate action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action candidates string predicate))))

  (defun az-org-log--read-destination ()
    "Prompt for a log destination and return its file path.
The journal candidate holds a symbol rather than a path so that merely
opening the prompt never creates a journal file."
    (let* ((candidates (cons (cons az-org-log-unassigned-label 'journal)
                             (az-org-log--project-alist)))
           (choice (completing-read "Log to: "
                                    (az-org-log--completion-table candidates)
                                    nil t))
           (target (cdr (assoc choice candidates))))
      (if (eq target 'journal)
          (az-org-log--journal-file)
        target)))

;;; --- finding or creating the day heading --------------------------------

  (defun az-org-log--day-stamp (date)
    "Return DATE as an inactive org timestamp at day precision.
Inactive brackets keep the log out of the agenda entirely."
    (format-time-string "[%Y-%m-%d %a]" date))

  (defun az-org-log--day-key (date)
    "Return DATE as a plain YYYY-MM-DD string, for comparison and sorting.
Keying on the date alone rather than the whole stamp keeps a locale
change in the day name from producing two headings for one day."
    (format-time-string "%Y-%m-%d" date))

  (defun az-org-log--goto-log-heading ()
    "Move point to the `* Log' heading, creating it at end of buffer if absent.
Always leaves point at the start of the heading line."
    (let ((found (org-find-exact-headline-in-buffer az-org-log-heading nil t)))
      (if found
          (goto-char found)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n* " az-org-log-heading "\n")
        (forward-line -1))))

  (defun az-org-log--day-headings ()
    "Return an alist of (DAY-KEY . POSITION) for the day headings under `* Log'.
Point must be on the `* Log' heading.  POSITION is the start of the
heading line.  Entries come back in buffer order, newest day first."
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          (result nil))
      (save-excursion
        (forward-line 1)
        (while (re-search-forward
                "^\\*\\* \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]*\\][ \t]*$"
                end t)
          (push (cons (match-string-no-properties 1) (line-beginning-position))
                result)))
      (nreverse result)))

  (defun az-org-log--goto-end-of-day (position)
    "Move point past the content of the day heading at POSITION.
Leaves point at the start of a fresh line, ready for a new item."
    (goto-char position)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (goto-char end)
      (skip-chars-backward " \t\n")
      (forward-line 1)
      (unless (bolp) (insert "\n"))))

  (defun az-org-log--insert-day (date key days)
    "Insert a day heading for DATE above the first day older than KEY.
DAYS is the alist from `az-org-log--day-headings'.  Point must be on the
`* Log' heading.  Leaves point on the line below the new heading."
    (let ((older (seq-find (lambda (day) (string< (car day) key)) days)))
      (if older
          (goto-char (cdr older))
        (goto-char (save-excursion (org-end-of-subtree t t) (point))))
      (unless (bolp) (insert "\n"))
      (insert "** " (az-org-log--day-stamp date) "\n")))

  (defun az-org-log--goto-day (date)
    "Move point to where a new entry for DATE belongs in the current buffer.
Create the `* Log' heading and DATE's day heading when they are missing."
    (az-org-log--goto-log-heading)
    (let* ((key (az-org-log--day-key date))
           (days (az-org-log--day-headings))
           (existing (assoc key days)))
      (if existing
          (az-org-log--goto-end-of-day (cdr existing))
        (az-org-log--insert-day date key days))))

  (defun az-org-log-goto-today ()
    "Jump to where today's log entry belongs in the current buffer."
    (interactive)
    (az-org-log--goto-day (current-time)))

;;; --- capture ------------------------------------------------------------

  (defun az-org-log-capture-target ()
    "Position point at today's entry in the Log tree of a chosen destination."
    (let ((file (az-org-log--read-destination)))
      (set-buffer (org-capture-target-buffer file))
      (widen)
      (goto-char (point-min))
      (az-org-log--goto-day (current-time))))

;;; --- reading the log back -----------------------------------------------

  (defun az-org-log--item-lines (beg end)
    "Return a list of (TEXT . POSITION) for the non blank lines between BEG and END."
    (let ((lines nil))
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let ((text (buffer-substring-no-properties
                       (line-beginning-position)
                       (min end (line-end-position)))))
            (unless (string-blank-p text)
              (push (cons text (line-beginning-position)) lines)))
          (forward-line 1)))
      (nreverse lines)))

  (defun az-org-log--day-entries (day-position)
    "Return the top level list items under the day heading at DAY-POSITION.
Each entry is a list of (TEXT . POSITION) lines, so multi line entries
and nested sub tasks survive intact."
    (save-excursion
      (goto-char day-position)
      (let ((end (save-excursion (org-end-of-subtree t t) (point)))
            (start (save-excursion (forward-line 1) (point)))
            (entries nil))
        (when (< start end)
          (save-restriction
            (narrow-to-region start end)
            (org-element-map (org-element-parse-buffer 'element) 'item
              (lambda (item)
                (push (az-org-log--item-lines
                       (org-element-property :begin item)
                       (org-element-property :end item))
                      entries))
              nil nil 'item)))
        (nreverse entries))))

  (defun az-org-log--parse-file (file cutoff label)
    "Return log records from FILE for days on or after CUTOFF.
Each record is (DAY-KEY LABEL FILE LINES).  FILE is read into a temp
buffer rather than visited: `org-refile-targets' is built from the open
org buffers, so visiting every project here would pollute it."
    (let ((records nil))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (let ((log-pos (org-find-exact-headline-in-buffer az-org-log-heading nil t)))
          (when log-pos
            (goto-char log-pos)
            (dolist (day (az-org-log--day-headings))
              (unless (string< (car day) cutoff)
                (dolist (entry (az-org-log--day-entries (cdr day)))
                  (push (list (car day) label file entry) records)))))))
      (nreverse records)))

  (defun az-org-log--cutoff-key ()
    "Return the oldest day key still inside the aggregation window."
    (az-org-log--day-key
     (time-subtract (current-time) (days-to-time az-org-log-days))))

  (defun az-org-log--journal-files-since (cutoff)
    "Return journal files whose denote identifier is on or after CUTOFF.
Journals accumulate for years, so skipping them by file name avoids
parsing hundreds of files on every refresh."
    (let ((compact (replace-regexp-in-string "-" "" cutoff)))
      (seq-filter
       (lambda (file)
         (let ((date (az-org-log--file-date file)))
           (or (null date) (not (string< date compact)))))
       (az-org-log--journal-files))))

  (defun az-org-log--record-lessp (a b)
    "Sort records so unassigned comes first, then labels alphabetically."
    (let ((la (nth 1 a))
          (lb (nth 1 b)))
      (cond
       ((equal la lb) nil)
       ((equal la az-org-log-unassigned-label) t)
       ((equal lb az-org-log-unassigned-label) nil)
       (t (string< la lb)))))

  (defun az-org-log--group-by-day (records)
    "Group RECORDS into (DAY-KEY . RECORDS) cells, newest day first."
    (let ((days nil))
      (dolist (record records)
        (let ((cell (assoc (nth 0 record) days)))
          (unless cell
            (setq cell (list (nth 0 record)))
            (push cell days))
          (setcdr cell (cons record (cdr cell)))))
      (dolist (cell days)
        (setcdr cell (sort (nreverse (cdr cell)) #'az-org-log--record-lessp)))
      (sort days (lambda (a b) (string> (car a) (car b))))))

  (defun az-org-log-collect ()
    "Return aggregated log records grouped by day, newest day first."
    (let ((cutoff (az-org-log--cutoff-key))
          (records nil))
      (dolist (file (az-org-log--project-files))
        (setq records (nconc records
                             (az-org-log--parse-file
                              file cutoff (az-org-log--display-name file)))))
      (dolist (file (az-org-log--journal-files-since cutoff))
        (setq records (nconc records
                             (az-org-log--parse-file
                              file cutoff az-org-log-unassigned-label))))
      (az-org-log--group-by-day records)))

;;; --- the panel ----------------------------------------------------------

  (defvar az-org-log-view-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r") #'az-org-log-show)
      (define-key map (kbd "g") #'az-org-log-show)
      (define-key map (kbd "q") #'quit-window)
      (define-key map (kbd "RET") #'az-org-log-goto-source)
      (define-key map (kbd "m") #'az-org-log-move-item)
      (define-key map (kbd "C-c C-c") #'az-org-log-toggle-checkbox)
      map)
    "Keymap for `az-org-log-view-mode'.")

  (define-derived-mode az-org-log-view-mode org-mode "Project Log"
    "Major mode for the aggregated project log panel."
    (setq buffer-read-only t))

  (with-eval-after-load 'evil
    (evil-set-initial-state 'az-org-log-view-mode 'motion)
    (evil-define-key 'motion az-org-log-view-mode-map
      (kbd "r") #'az-org-log-show
      (kbd "q") #'quit-window
      (kbd "RET") #'az-org-log-goto-source
      (kbd "m") #'az-org-log-move-item))

  (defun az-org-log--format-day-header (key)
    "Return KEY, a YYYY-MM-DD string, as a readable day header."
    (format-time-string "%Y-%m-%d %A" (date-to-time (concat key " 00:00:00"))))

  (defun az-org-log--insert-line (text source)
    "Insert TEXT as one panel line carrying SOURCE as a text property.
SOURCE is recorded per line, not per entry, so that the checkbox toggle
can address a specific nested sub task."
    (let ((start (point)))
      (insert text "\n")
      (put-text-property start (point) 'az-org-log-source source)))

  (defun az-org-log--render ()
    "Rebuild the aggregated log buffer and return it."
    (let* ((days (az-org-log-collect))
           (buffer (get-buffer-create az-org-log-buffer))
           (line (with-current-buffer buffer (line-number-at-pos))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'az-org-log-view-mode)
            (az-org-log-view-mode))
          (if (null days)
              (insert (format "No log entries in the last %d days.\n"
                              az-org-log-days))
            (dolist (day days)
              (insert (az-org-log--format-day-header (car day)) "\n\n")
              (let ((label nil))
                (dolist (record (cdr day))
                  (unless (equal label (nth 1 record))
                    (setq label (nth 1 record))
                    (insert "  " label "\n"))
                  (dolist (line (nth 3 record))
                    (az-org-log--insert-line
                     (concat "  " (car line))
                     (cons (nth 2 record) (cdr line))))))
              (insert "\n")))
          (goto-char (point-min))
          (forward-line (1- line))))
      buffer))

  (defun az-org-log-show ()
    "Rebuild and display the aggregated log panel."
    (interactive)
    (display-buffer (az-org-log--render)))

  (defun az-org-log-refresh ()
    "Rebuild the aggregated log panel when it already exists."
    (when (get-buffer az-org-log-buffer)
      (az-org-log--render)))

  (defun az-org-log-toggle ()
    "Show or hide the aggregated log panel."
    (interactive)
    (let ((window (get-buffer-window az-org-log-buffer)))
      (if window
          (quit-window nil window)
        (az-org-log-show))))

  (defun az-org-log-dashboard ()
    "Show the custom agenda with the aggregated log panel beside it."
    (interactive)
    (az/custom-agenda)
    (az-org-log-show))

;;; --- acting on a panel line ---------------------------------------------

  (defun az-org-log--source-at-point ()
    "Return the (FILE . POSITION) recorded for the panel line at point."
    (or (get-text-property (point) 'az-org-log-source)
        (user-error "No log entry on this line")))

  (defun az-org-log-goto-source ()
    "Visit the source of the log line at point."
    (interactive)
    (let ((source (az-org-log--source-at-point)))
      (pop-to-buffer (find-file-noselect (car source)))
      (widen)
      (goto-char (cdr source))
      (cond ((fboundp 'org-fold-show-context) (org-fold-show-context))
            ((fboundp 'org-show-context) (org-show-context)))))

  (defun az-org-log-toggle-checkbox ()
    "Toggle the checkbox of the panel line at point in its source file."
    (interactive)
    (let ((source (az-org-log--source-at-point)))
      (with-current-buffer (find-file-noselect (car source))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (cdr source))
            (beginning-of-line)
            (unless (org-at-item-checkbox-p)
              (user-error "No checkbox on this line"))
            (org-toggle-checkbox)))
        (save-buffer))
      (az-org-log--render)))

;;; --- promoting an entry to a project ------------------------------------

  (defun az-org-log--top-level-item-bounds ()
    "Return (BEGIN . END) for the top level list item around point, or nil.
Climbing to the outermost item means invoking this on a nested sub task
moves the whole entry rather than orphaning that line."
    (let ((item (org-element-lineage (org-element-at-point) '(item) t))
          (outer nil))
      (while item
        (setq outer item
              item (org-element-lineage item '(item))))
      (when outer
        (cons (org-element-property :begin outer)
              (org-element-property :end outer)))))

  (defun az-org-log--current-day-key ()
    "Return the day key of the log day heading containing point, or nil."
    (save-excursion
      (when (ignore-errors (org-back-to-heading t) t)
        (when (looking-at
               "^\\*\\* \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]*\\]")
          (match-string-no-properties 1)))))

  (defun az-org-log--delete-day-if-empty (day-key)
    "Delete DAY-KEY's heading in the current buffer when nothing is left under it."
    (save-excursion
      (goto-char (point-min))
      (let ((log-pos (org-find-exact-headline-in-buffer az-org-log-heading nil t)))
        (when log-pos
          (goto-char log-pos)
          (let ((day (assoc day-key (az-org-log--day-headings))))
            (when day
              (goto-char (cdr day))
              (let* ((start (point))
                     (end (save-excursion (org-end-of-subtree t t) (point)))
                     (body (save-excursion
                             (forward-line 1)
                             (buffer-substring-no-properties (min (point) end) end))))
                (when (string-blank-p body)
                  (delete-region start end)))))))))

  (defun az-org-log-move-item ()
    "Move the log entry at point to another destination, keeping its date.
`org-refile' cannot do this because log entries are plain list items
rather than headings."
    (interactive)
    (when (derived-mode-p 'az-org-log-view-mode)
      (az-org-log-goto-source))
    (let ((bounds (az-org-log--top-level-item-bounds))
          (day-key (az-org-log--current-day-key)))
      (unless bounds (user-error "Point is not inside a log entry"))
      (unless day-key (user-error "Point is not under a log day heading"))
      (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (source (current-buffer))
             (target (az-org-log--read-destination)))
        (when (equal (expand-file-name target)
                     (expand-file-name (or (buffer-file-name) "")))
          (user-error "That entry is already in this file"))
        (with-current-buffer (find-file-noselect target)
          (save-excursion
            (save-restriction
              (widen)
              (az-org-log--goto-day (date-to-time (concat day-key " 00:00:00")))
              (insert text)))
          (save-buffer))
        (with-current-buffer source
          (delete-region (car bounds) (cdr bounds))
          (az-org-log--delete-day-if-empty day-key)
          (save-buffer))
        (az-org-log-refresh)
        (message "Moved log entry to %s" (az-org-log--display-name target)))))

;;; --- wiring -------------------------------------------------------------

  (add-to-list 'display-buffer-alist
               `(,(regexp-quote az-org-log-buffer)
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 60)
                 (slot . 0)))

  (add-hook 'org-capture-after-finalize-hook #'az-org-log-refresh)

  ;; keeps `C-c n t' honest too, not just the log
  (advice-add 'denote-journal-new-or-existing-entry :before
              #'az-org-log--sync-journal-directory)
  (advice-add 'denote-journal-path-to-new-or-existing-entry :before
              #'az-org-log--sync-journal-directory)

  (global-set-key (kbd "<f11>") #'az-org-log-toggle)
  (global-set-key (kbd "C-c n d") #'az-org-log-dashboard))

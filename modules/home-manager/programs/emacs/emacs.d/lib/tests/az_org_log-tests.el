;; -*- lexical-binding: t; -*-
;; Run with:
;;   emacs -Q --batch -l tests/az_org_log-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'org)

(setenv "TZ" "Europe/Zurich")

;; az_org_log.el is gated on enable-notes and pulls in denote.  Only the
;; display name path touches denote, and that is not exercised here, so a
;; stub keeps these tests runnable without the package set.
(setq enable-notes t)
(unless (require 'denote-journal nil t) (provide 'denote-journal))
(defvar az-org-projects-dir "/tmp/az-org-log-tests/02_projects/")
(defvar az-org-archive-dir "/tmp/az-org-log-tests/99_archive/")

(load (expand-file-name "../az_org_log.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro az-org-log-with-org (content &rest body)
  "Run BODY in a temporary org buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (delay-mode-hooks (org-mode))
     (goto-char (point-min))
     ,@body))

(defun az-org-log-test-day-keys ()
  "Return the day keys under `* Log' in the current buffer."
  (goto-char (point-min))
  (az-org-log--goto-log-heading)
  (mapcar #'car (az-org-log--day-headings)))

;; --- pure layer ---------------------------------------------------------

(ert-deftest az-org-log-day-key ()
  (should (equal (az-org-log--day-key (date-to-time "2026-08-10 00:00:00"))
                 "2026-08-10")))

(ert-deftest az-org-log-day-stamp-is-inactive ()
  (should (equal (az-org-log--day-stamp (date-to-time "2026-08-10 00:00:00"))
                 "[2026-08-10 Mon]")))

(ert-deftest az-org-log-day-header-is-readable ()
  (should (equal (az-org-log--format-day-header "2026-08-10") "2026-08-10 Monday")))

(ert-deftest az-org-log-file-date-from-denote-identifier ()
  (should (equal (az-org-log--file-date "/j/20260810T120000--foo__journal.org")
                 "20260810")))

(ert-deftest az-org-log-file-date-absent ()
  (should-not (az-org-log--file-date "/p/00_personal.org")))

(ert-deftest az-org-log-journal-files-since-skips-old-years ()
  (cl-letf (((symbol-function 'az-org-log--journal-files)
             (lambda () '("/j/20260810T090000--a__journal.org"
                          "/j/20250101T090000--b__journal.org"))))
    (should (equal (az-org-log--journal-files-since "2026-08-01")
                   '("/j/20260810T090000--a__journal.org")))))

(ert-deftest az-org-log-days-sort-newest-first ()
  (let ((records (list (list "2026-07-01" "a" "f" nil)
                       (list "2026-08-10" "a" "f" nil))))
    (should (equal (mapcar #'car (az-org-log--group-by-day records))
                   '("2026-08-10" "2026-07-01")))))

(ert-deftest az-org-log-unassigned-sorts-first-within-a-day ()
  (let ((records (list (list "2026-08-10" "zebra" "f" nil)
                       (list "2026-08-10" az-org-log-unassigned-label "j" nil)
                       (list "2026-08-10" "alpha" "f" nil))))
    (should (equal (mapcar (lambda (record) (nth 1 record))
                           (cdr (car (az-org-log--group-by-day records))))
                   (list az-org-log-unassigned-label "alpha" "zebra")))))

;; --- the Log heading ----------------------------------------------------

(ert-deftest az-org-log-creates-log-heading-once ()
  (az-org-log-with-org "#+title: Test\n"
    (az-org-log--goto-log-heading)
    (should (looking-at "^\\* Log$"))
    (az-org-log--goto-log-heading)
    (should (looking-at "^\\* Log$"))
    (goto-char (point-min))
    (should (= 1 (how-many "^\\* Log$")))))

(ert-deftest az-org-log-creates-log-heading-without-trailing-newline ()
  (az-org-log-with-org "#+title: Test"
    (az-org-log--goto-log-heading)
    (should (looking-at "^\\* Log$"))))

(ert-deftest az-org-log-day-headings-stop-at-next-heading ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- a\n* Tasks\n** [2026-07-01 Wed]\n"
    (should (equal (az-org-log-test-day-keys) '("2026-08-10")))))

(ert-deftest az-org-log-day-headings-empty-log ()
  (az-org-log-with-org "* Log\n"
    (should-not (az-org-log-test-day-keys))))

;; --- placing a new entry ------------------------------------------------

(ert-deftest az-org-log-new-day-goes-on-top ()
  (az-org-log-with-org "* Log\n** [2026-08-01 Sat]\n- old\n"
    (az-org-log--goto-day (date-to-time "2026-08-10 00:00:00"))
    (insert "- new\n")
    (should (equal (az-org-log-test-day-keys) '("2026-08-10" "2026-08-01")))))

(ert-deftest az-org-log-older-day-goes-below ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- new\n"
    (az-org-log--goto-day (date-to-time "2026-07-01 00:00:00"))
    (insert "- old\n")
    (should (equal (az-org-log-test-day-keys) '("2026-08-10" "2026-07-01")))))

(ert-deftest az-org-log-middle-day-slots-in ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- a\n** [2026-07-01 Wed]\n- b\n"
    (az-org-log--goto-day (date-to-time "2026-08-05 00:00:00"))
    (insert "- c\n")
    (should (equal (az-org-log-test-day-keys)
                   '("2026-08-10" "2026-08-05" "2026-07-01")))))

(ert-deftest az-org-log-existing-day-is-reused ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n"
    (az-org-log--goto-day (date-to-time "2026-08-10 00:00:00"))
    (insert "- second\n")
    (should (equal (az-org-log-test-day-keys) '("2026-08-10")))))

(ert-deftest az-org-log-appends-below-a-nested-entry ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n  - [ ] sub\n"
    (az-org-log--goto-day (date-to-time "2026-08-10 00:00:00"))
    (insert "- second\n")
    (should (equal (buffer-string)
                   "* Log\n** [2026-08-10 Mon]\n- first\n  - [ ] sub\n- second\n"))))

(ert-deftest az-org-log-creates-log-and-day-together ()
  (az-org-log-with-org "#+title: Test\n"
    (az-org-log--goto-day (date-to-time "2026-08-10 00:00:00"))
    (insert "- only\n")
    (should (equal (az-org-log-test-day-keys) '("2026-08-10")))))

;; --- reading entries back -----------------------------------------------

(ert-deftest az-org-log-day-entries-keep-nesting ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n  - [ ] sub\n- second\n"
    (az-org-log--goto-log-heading)
    (let ((entries (az-org-log--day-entries (cdr (car (az-org-log--day-headings))))))
      (should (= (length entries) 2))
      (should (equal (mapcar #'car (nth 0 entries)) '("- first" "  - [ ] sub")))
      (should (equal (mapcar #'car (nth 1 entries)) '("- second"))))))

(ert-deftest az-org-log-day-entries-keep-continuation-lines ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n  more text\n"
    (az-org-log--goto-log-heading)
    (let ((entries (az-org-log--day-entries (cdr (car (az-org-log--day-headings))))))
      (should (equal (mapcar #'car (nth 0 entries)) '("- first" "  more text"))))))

(ert-deftest az-org-log-day-entries-positions-point-at-source ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n  - [ ] sub\n"
    (az-org-log--goto-log-heading)
    (let* ((entries (az-org-log--day-entries (cdr (car (az-org-log--day-headings)))))
           (sub (nth 1 (nth 0 entries))))
      (goto-char (cdr sub))
      (should (looking-at "  - \\[ \\] sub")))))

(ert-deftest az-org-log-day-entries-empty-day ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n"
    (az-org-log--goto-log-heading)
    (should-not (az-org-log--day-entries (cdr (car (az-org-log--day-headings)))))))

;; --- promoting an entry -------------------------------------------------

(ert-deftest az-org-log-outer-item-found-from-nested-line ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n  - [ ] sub\n- second\n"
    (search-forward "[ ] sub")
    (let ((bounds (az-org-log--top-level-item-bounds)))
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "- first\n  - [ ] sub\n")))))

(ert-deftest az-org-log-current-day-key-from-item ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- first\n"
    (search-forward "first")
    (should (equal (az-org-log--current-day-key) "2026-08-10"))))

(ert-deftest az-org-log-empty-day-heading-removed ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n** [2026-08-01 Sat]\n- keep\n"
    (az-org-log--delete-day-if-empty "2026-08-10")
    (should (equal (az-org-log-test-day-keys) '("2026-08-01")))))

(ert-deftest az-org-log-non-empty-day-heading-kept ()
  (az-org-log-with-org "* Log\n** [2026-08-10 Mon]\n- still here\n"
    (az-org-log--delete-day-if-empty "2026-08-10")
    (should (equal (az-org-log-test-day-keys) '("2026-08-10")))))

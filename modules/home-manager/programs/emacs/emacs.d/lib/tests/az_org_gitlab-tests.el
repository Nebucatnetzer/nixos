;; -*- lexical-binding: t; -*-
(require 'ert)

(setenv "TZ" "Europe/Zurich")

;; --- pure layer ---------------------------------------------------------

(ert-deftest az/gitlab-priority-high () (should (equal (az/gitlab--issue-priority '("bug" "priority::high")) "A")))
(ert-deftest az/gitlab-priority-normal () (should (equal (az/gitlab--issue-priority '("priority::normal")) "B")))
(ert-deftest az/gitlab-priority-low () (should (equal (az/gitlab--issue-priority '("priority::low" "ui")) "C")))
(ert-deftest az/gitlab-priority-unknown () (should-not (az/gitlab--issue-priority '("bug" "ui"))))
(ert-deftest az/gitlab-priority-empty () (should-not (az/gitlab--issue-priority nil)))

(ert-deftest az/gitlab-project-from-references ()
  (should (equal (az/gitlab--issue-project '((references (full . "grp/alpha#42")))) "grp/alpha")))
(ert-deftest az/gitlab-project-from-web-url-fallback ()
  (should (equal (az/gitlab--issue-project
                  '((web_url . "https://gl.example.com/grp/alpha/-/issues/42")))
                 "grp/alpha")))
(ert-deftest az/gitlab-project-from-web-url-nested ()
  (should (equal (az/gitlab--issue-project
                  '((web_url . "https://gl.example.com/grp/sub/beta/-/issues/7")))
                 "grp/sub/beta")))

(ert-deftest az/gitlab-slug-simple () (should (equal (az/gitlab--project-slug "grp/alpha") "alpha")))
(ert-deftest az/gitlab-slug-nested () (should (equal (az/gitlab--project-slug "grp/sub/beta") "beta")))
(ert-deftest az/gitlab-slug-bare () (should (equal (az/gitlab--project-slug "solo") "solo")))

(ert-deftest az/gitlab-timestamp-winter ()   ; UTC+1
  (should (equal (az/gitlab--org-timestamp "2026-01-15T08:12:03.000Z") "[2026-01-15 Thu 09:12]")))
(ert-deftest az/gitlab-timestamp-summer ()   ; UTC+2, catches a DST bug
  (should (equal (az/gitlab--org-timestamp "2026-08-20T08:12:03.000Z") "[2026-08-20 Thu 10:12]")))

(defconst az/gitlab-test-json
  "[{\"title\":\"First\",\"web_url\":\"https://gl/grp/alpha/-/issues/1\",
     \"labels\":[\"priority::high\",\"bug\"],\"created_at\":\"2026-01-15T08:12:03.000Z\",
     \"references\":{\"full\":\"grp/alpha#1\"}},
    {\"title\":\"Second\",\"web_url\":\"https://gl/grp/sub/beta/-/issues/2\",
     \"labels\":[],\"created_at\":\"2026-08-20T08:12:03.000Z\",
     \"references\":{\"full\":\"grp/sub/beta#2\"}}]")

(ert-deftest az/gitlab-parse-issues ()
  (let ((issues (az/gitlab--parse-issues az/gitlab-test-json)))
    (should (= (length issues) 2))
    (should (equal (plist-get (nth 0 issues) :title) "First"))
    (should (equal (plist-get (nth 0 issues) :project) "grp/alpha"))
    (should (equal (plist-get (nth 0 issues) :priority) "A"))
    (should (equal (plist-get (nth 0 issues) :url) "https://gl/grp/alpha/-/issues/1"))
    (should (equal (plist-get (nth 1 issues) :project) "grp/sub/beta"))
    (should-not (plist-get (nth 1 issues) :priority))))

(ert-deftest az/gitlab-heading-string-with-priority ()
  (should (equal (az/gitlab--heading-string
                  '(:url "https://gl/a/b/-/issues/1" :title "Fix it"
                    :priority "A" :created "[2026-08-20 Thu 10:12]") 2)
                 (concat "** TODO [#A] Fix it\n:PROPERTIES:\n"
                         ":URL: https://gl/a/b/-/issues/1\n"
                         ":CREATED: [2026-08-20 Thu 10:12]\n:END:\n"))))

(ert-deftest az/gitlab-heading-string-without-priority ()
  (should (equal (az/gitlab--heading-string
                  '(:url "u" :title "Plain" :priority nil :created "[2026-08-20 Thu 10:12]") 2)
                 "** TODO Plain\n:PROPERTIES:\n:URL: u\n:CREATED: [2026-08-20 Thu 10:12]\n:END:\n")))

(ert-deftest az/gitlab-base-url-refuses-empty-host ()
  (let ((az-gitlab-host ""))
    (should-error (az/gitlab--base-url) :type 'user-error)))

;; --- buffer layer (no network) ------------------------------------------

(defmacro az/gitlab-with-org (initial &rest body)
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "*gl-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,initial)
           (org-mode)
           (goto-char (point-min))
           ,@body)
       ;; A live clock makes `kill-buffer' prompt via `org-check-running-clock',
       ;; which blocks forever on stdin under --batch.
       (with-current-buffer buf
         (when (org-clocking-p)
           (let ((org-log-note-clock-out nil))
             (org-clock-out nil t))))
       (let ((kill-buffer-query-functions nil))
         (kill-buffer buf)))))

(ert-deftest az/gitlab-existing-urls-collects-all ()
  (az/gitlab-with-org "* g/p\n** TODO one\n:PROPERTIES:\n:URL: u1\n:END:\n** TODO two\n:PROPERTIES:\n:URL: u2\n:END:\n"
    (let ((urls (az/gitlab--existing-urls)))
      (should (gethash "u1" urls))
      (should (gethash "u2" urls))
      (should-not (gethash "u3" urls))
      (should (= (hash-table-count urls) 2)))))

(ert-deftest az/gitlab-project-heading-creates-with-category ()
  (az/gitlab-with-org "#+title: GitLab\n"
    (az/gitlab--project-heading "grp/sub/beta")
    (should (string-match-p "^\\* grp/sub/beta$" (buffer-string)))
    (should (string-match-p "^:CATEGORY: beta$" (buffer-string)))
    ;; second call must reuse, not duplicate
    (az/gitlab--project-heading "grp/sub/beta")
    (should (= 1 (cl-count "grp/sub/beta" (split-string (buffer-string) "\n")
                           :test (lambda (a b) (equal b (concat "* " a))))))))

(ert-deftest az/gitlab-category-inherits-to-issue ()
  (az/gitlab-with-org "#+title: GitLab\n"
    (az/gitlab--insert-issue '(:url "u1" :title "One" :priority "A"
                               :created "[2026-08-20 Thu 10:12]" :project "grp/alpha"))
    (goto-char (point-min))
    (search-forward "One")
    (should (equal (org-get-category) "alpha"))))

(ert-deftest az/gitlab-insert-groups-under-project ()
  (az/gitlab-with-org "#+title: GitLab\n"
    (dolist (i '((:url "u1" :title "A1" :priority "A" :created "[2026-08-20 Thu 10:12]" :project "grp/alpha")
                 (:url "u2" :title "B1" :priority nil :created "[2026-08-20 Thu 10:12]" :project "grp/beta")
                 (:url "u3" :title "A2" :priority "C" :created "[2026-08-20 Thu 10:12]" :project "grp/alpha")))
      (az/gitlab--insert-issue i))
    (let ((lines (seq-filter (lambda (l) (string-prefix-p "*" l))
                             (split-string (buffer-string) "\n"))))
      (should (equal lines '("* grp/alpha"
                             "** TODO [#A] A1"
                             "** TODO [#C] A2"
                             "* grp/beta"
                             "** TODO B1"))))))

(ert-deftest az/gitlab-close-stale-marks-done-and-skips-known ()
  (az/gitlab-with-org "* g/p\n** TODO keep\n:PROPERTIES:\n:URL: u1\n:END:\n** TODO gone\n:PROPERTIES:\n:URL: u2\n:END:\n** DONE already\n:PROPERTIES:\n:URL: u3\n:END:\n"
    (let ((known (make-hash-table :test 'equal)))
      (puthash "u1" t known)
      (should (= 1 (az/gitlab--close-stale known)))
      (should (string-match-p "^\\*\\* TODO keep$" (buffer-string)))
      (should (string-match-p "^\\*\\* DONE gone$" (buffer-string))))))

(ert-deftest az/gitlab-close-stale-rescues-running-clock ()
  (az/gitlab-with-org "* g/p\n** TODO gone\n:PROPERTIES:\n:URL: u2\n:END:\n"
    (goto-char (point-min))
    (search-forward "gone")
    (org-clock-in)
    (should (az/gitlab--clocking-here-p))
    (let ((known (make-hash-table :test 'equal))
          (rescued nil))
      ;; stub the work-file clock so the test needs no Nextcloud tree
      (cl-letf (((symbol-function 'start-main-clock) (lambda () (setq rescued t))))
        (az/gitlab--close-stale known))
      (should rescued)
      (should (string-match-p "^\\*\\* DONE gone$" (buffer-string))))))

(ert-deftest az/gitlab-close-stale-leaves-clock-alone-elsewhere ()
  (az/gitlab-with-org "* g/p\n** TODO clocked\n:PROPERTIES:\n:URL: u1\n:END:\n** TODO gone\n:PROPERTIES:\n:URL: u2\n:END:\n"
    (goto-char (point-min))
    (search-forward "clocked")
    (org-clock-in)
    (let ((known (make-hash-table :test 'equal))
          (rescued nil))
      (puthash "u1" t known)
      (cl-letf (((symbol-function 'start-main-clock) (lambda () (setq rescued t))))
        (az/gitlab--close-stale known))
      (should-not rescued)                ; clock was never on the closed entry
      (should (az/gitlab-test-clock-live-p)))))

(defun az/gitlab-test-clock-live-p () (org-clocking-p))

(ert-deftest az/gitlab-close-stale-warns-when-clock-mover-missing ()
  "Losing the clock must be loud, never silent."
  (az/gitlab-with-org "* g/p\n** TODO gone\n:PROPERTIES:\n:URL: u2\n:END:\n"
    (goto-char (point-min))
    (search-forward "gone")
    (org-clock-in)
    (let ((known (make-hash-table :test 'equal))
          (warned nil))
      (cl-letf (((symbol-function 'start-main-clock) nil)
                ((symbol-function 'warn) (lambda (&rest _) (setq warned t))))
        (az/gitlab--close-stale known))
      (should warned)
      (should (string-match-p "^\\*\\* DONE gone$" (buffer-string))))))

;; --- updating existing headings -----------------------------------------

(defconst az/gitlab-test-furnished
  (concat "* grp/alpha\n:PROPERTIES:\n:CATEGORY: alpha\n:END:\n"
          "** DONE [#C] Stale title  :mytag:urgent:\n"
          "CLOSED: [2026-08-19 Wed 12:00] SCHEDULED: <2026-08-25 Tue 09:00> DEADLINE: <2026-08-28 Fri>\n"
          ":PROPERTIES:\n:URL: u1\n:CREATED: [2026-08-01 Sat 09:00]\n:END:\n"
          ":LOGBOOK:\nCLOCK: [2026-08-18 Tue 10:00]--[2026-08-18 Tue 11:30] =>  1:30\n:END:\n"
          "My own notes with a [[https://example.com][link]].\n\n"
          "*** TODO a subtask I added myself\n")
  "A heading carrying everything the sync must not disturb.")

(defun az/gitlab-test-goto (text)
  (goto-char (point-min))
  (search-forward text)
  (org-back-to-heading t))

(ert-deftest az/gitlab-update-preserves-scheduling-and-notes ()
  "Scheduling, notes and clock history are mine; the sync must not touch them."
  (az/gitlab-with-org az/gitlab-test-furnished
    (az/gitlab-test-goto "Stale title")
    (should (az/gitlab--update-issue
             '(:url "u1" :title "Fresh title" :priority "A" :project "grp/alpha")))
    (let ((result (buffer-string)))
      ;; updated
      (should (string-match-p "^\\*\\* TODO \\[#A\\] Fresh title" result))
      ;; preserved
      (should (string-match-p "SCHEDULED: <2026-08-25 Tue 09:00>" result))
      (should (string-match-p "DEADLINE: <2026-08-28 Fri>" result))
      (should (string-match-p ":mytag:urgent:" result))
      (should (string-match-p "CLOCK: \\[2026-08-18 Tue 10:00\\]--\\[2026-08-18 Tue 11:30\\] =>  1:30" result))
      (should (string-match-p "My own notes with a \\[\\[https://example.com\\]\\[link\\]\\]" result))
      (should (string-match-p "^\\*\\*\\* TODO a subtask I added myself$" result))
      (should (string-match-p ":CREATED: \\[2026-08-01 Sat 09:00\\]" result))
      ;; reopening must drop the CLOSED stamp
      (should-not (string-match-p "CLOSED:" result)))))

(ert-deftest az/gitlab-update-is-a-noop-when-nothing-changed ()
  (az/gitlab-with-org "* grp/alpha\n** TODO [#B] Same title\n:PROPERTIES:\n:URL: u1\n:END:\n"
    (az/gitlab-test-goto "Same title")
    (let ((before (buffer-string)))
      (should-not (az/gitlab--update-issue
                   '(:url "u1" :title "Same title" :priority "B" :project "grp/alpha")))
      (should (equal before (buffer-string))))))

(ert-deftest az/gitlab-update-removes-priority-when-label-dropped ()
  (az/gitlab-with-org "* grp/alpha\n** TODO [#A] Title\n:PROPERTIES:\n:URL: u1\n:END:\n"
    (az/gitlab-test-goto "Title")
    (should (az/gitlab--update-issue
             '(:url "u1" :title "Title" :priority nil :project "grp/alpha")))
    (should (string-match-p "^\\*\\* TODO Title$" (buffer-string)))))

(ert-deftest az/gitlab-update-adds-priority-when-absent ()
  (az/gitlab-with-org "* grp/alpha\n** TODO Title\n:PROPERTIES:\n:URL: u1\n:END:\n"
    (az/gitlab-test-goto "Title")
    (should (az/gitlab--update-issue
             '(:url "u1" :title "Title" :priority "C" :project "grp/alpha")))
    (should (string-match-p "^\\*\\* TODO \\[#C\\] Title$" (buffer-string)))))

(ert-deftest az/gitlab-update-keeps-my-in-progress-keyword ()
  "Only done states are reopened; NEXT and WAITING are my own workflow."
  (dolist (keyword '("NEXT" "WAITING" "PROJECT"))
    (az/gitlab-with-org (format "* grp/alpha\n** %s [#B] Title\n:PROPERTIES:\n:URL: u1\n:END:\n" keyword)
      (az/gitlab-test-goto "Title")
      (should-not (az/gitlab--update-issue
                   '(:url "u1" :title "Title" :priority "B" :project "grp/alpha")))
      (should (equal (org-get-todo-state) keyword)))))

(ert-deftest az/gitlab-update-reopens-cancelled ()
  (az/gitlab-with-org "* grp/alpha\n** CANCELLED [#B] Title\n:PROPERTIES:\n:URL: u1\n:END:\n"
    (az/gitlab-test-goto "Title")
    (should (az/gitlab--update-issue
             '(:url "u1" :title "Title" :priority "B" :project "grp/alpha")))
    (should (equal (org-get-todo-state) "TODO"))))

(ert-deftest az/gitlab-existing-urls-yields-usable-markers ()
  (az/gitlab-with-org "* grp/alpha\n** TODO one\n:PROPERTIES:\n:URL: u1\n:END:\n** TODO two\n:PROPERTIES:\n:URL: u2\n:END:\n"
    (let ((marker (gethash "u2" (az/gitlab--existing-urls))))
      (should (markerp marker))
      (goto-char (marker-position marker))
      (should (equal (nth 4 (org-heading-components)) "two")))))

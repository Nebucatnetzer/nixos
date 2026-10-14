;; -*- lexical-binding: t; -*-
;; Run with:
;;   ORG=$(ls -d /nix/store/*-emacs-org-9.8.*/share/emacs/site-lisp/elpa/org-9.8.* | head -1)
;;   emacs -Q --batch --eval "(add-to-list 'load-path \"$ORG\")" --load tests/az_org_clocking-tests.el --funcall ert-run-tests-batch-and-exit
(require 'ert)
(require 'org)
(require 'org-clock)

(setenv "TZ" "Europe/Zurich")

;; az_org_clocking.el is gated on enable-clocking.
(setq enable-clocking t)
(defvar az-org-work-file "/tmp/az-org-clocking-tests/work.org")

(load (expand-file-name "../az_org_clocking.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; org-clock-persistence-insinuate saves to ~/.emacs.d on exit, which fails in batch.
(remove-hook 'kill-emacs-hook #'org-clock-save)

(defun az-org-clocking-test-row (level headline minutes &optional cost-center factor)
  "Build a row like `org-clock-get-table-data' returns it."
  (let ((properties nil))
    (when factor
      (push (cons "CLOCK_FACTOR" factor) properties))
    (when cost-center
      (push (cons "COST_CENTER" cost-center) properties))
    (list level headline nil nil minutes properties)))

(defun az-org-clocking-test-factored (table)
  "Return (GROUP FACTORED-MINUTES) for every group row of TABLE."
  (let ((factored nil))
    (dolist (row (nth 2 table))
      (when (= (nth 0 row) 1)
        (push (list (nth 1 row) (cdr (assoc "Factored" (nth 5 row)))) factored)))
    (nreverse factored)))

(defun az-org-clocking-test-summary (table)
  "Return (LEVEL HEADLINE MINUTES) for every row of TABLE."
  (let ((summary nil))
    (dolist (row (nth 2 table))
      (push (list (nth 0 row) (nth 1 row) (nth 4 row)) summary))
    (nreverse summary)))

(ert-deftest az-org-clocking-merges-projects-on-same-cost-center ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 90
                            (list (az-org-clocking-test-row 1 "ERP" 60 "4711")
                                  (az-org-clocking-test-row 1 "DevOps" 30 "4711")))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 90)
                     (2 "ERP" 60)
                     (2 "DevOps" 30))))))

(ert-deftest az-org-clocking-puts-rows-without-cost-center-last ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 50
                            (list (az-org-clocking-test-row 1 "Repeating" 20)
                                  (az-org-clocking-test-row 1 "ERP" 30 "4711")))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 30)
                     (2 "ERP" 30)
                     (1 "No cost center" 20)
                     (2 "Repeating" 20))))))

(ert-deftest az-org-clocking-keeps-inherited-children-in-group ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 60
                            (list (az-org-clocking-test-row 1 "ERP" 60 "4711")
                                  (az-org-clocking-test-row 2 "Review" 45 "4711")
                                  (az-org-clocking-test-row 3 "Coverage" 15 "4711")))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 60)
                     (2 "ERP" 60)
                     (3 "Review" 45)
                     (4 "Coverage" 15))))))

(ert-deftest az-org-clocking-moves-nested-cost-center-out-of-parent ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 150
                            (list (az-org-clocking-test-row 1 "Work Tasks" 150)
                                  (az-org-clocking-test-row 2 "ERP" 100 "4711")
                                  (az-org-clocking-test-row 3 "Review" 40 "4711")
                                  (az-org-clocking-test-row 2 "Mail" 20)))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 100)
                     (2 "ERP" 100)
                     (3 "Review" 40)
                     (1 "No cost center" 50)
                     (2 "Work Tasks" 50)
                     (3 "Mail" 20))))))

(ert-deftest az-org-clocking-drops-parent-without-own-time ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 60
                            (list (az-org-clocking-test-row 1 "Work" 60)
                                  (az-org-clocking-test-row 2 "ERP" 60 "4711")))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 60)
                     (2 "ERP" 60))))))

(ert-deftest az-org-clocking-counts-alternating-cost-centers-once ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 100
                            (list (az-org-clocking-test-row 1 "A" 100 "X")
                                  (az-org-clocking-test-row 2 "B" 60 "Y")
                                  (az-org-clocking-test-row 3 "C" 30 "X")
                                  (az-org-clocking-test-row 4 "D" 10 "Y")))))))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "X" 60)
                     (2 "A" 40)
                     (2 "C" 20)
                     (1 "Y" 40)
                     (2 "B" 30)
                     (2 "D" 10))))))

(ert-deftest az-org-clocking-merges-groups-across-files ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 30 (list (az-org-clocking-test-row 1 "ERP" 30 "4711")))
                      (list "b.org" 10 (list (az-org-clocking-test-row 1 "Infra" 10 "4711")))))))
    (should (equal (nth 1 table) 40))
    (should (equal (az-org-clocking-test-summary table)
                   '((1 "4711" 40)
                     (2 "ERP" 30)
                     (2 "Infra" 10))))))

(ert-deftest az-org-clocking-handles-empty-tables ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 0 nil)))))
    (should (equal (nth 1 table) 0))
    (should (null (nth 2 table)))))

(ert-deftest az-org-clocking-factored-sum-defaults-to-clocked-time ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 60
                            (list (az-org-clocking-test-row 1 "ERP" 60 "4711")))))))
    (should (equal (az-org-clocking-test-factored table)
                   '(("4711" 60))))))

(ert-deftest az-org-clocking-applies-inherited-factor ()
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 120
                            (list (az-org-clocking-test-row 1 "Mo-Fri 20-23" 120 "After hours" "1.25")
                                  (az-org-clocking-test-row 2 "Patch" 40 "After hours" "1.25")))))))
    (should (equal (az-org-clocking-test-factored table)
                   '(("After hours" 150.0))))))

(ert-deftest az-org-clocking-applies-each-factor-to-own-time ()
  ;; The parent has 40 minutes of its own at 1.0, the child 20 minutes at 1.5.
  (let ((table (az--org-clocktable-group-by-cost-center
                (list (list "a.org" 60
                            (list (az-org-clocking-test-row 1 "After hours" 60 "AH")
                                  (az-org-clocking-test-row 2 "Night" 20 "AH" "1.5")))))))
    (should (equal (az-org-clocking-test-factored table)
                   '(("AH" 70.0))))))

(ert-deftest az-org-clocking-rejects-invalid-factor ()
  (should-error (az--org-clocktable-group-by-cost-center
                 (list (list "a.org" 60
                             (list (az-org-clocking-test-row 1 "ERP" 60 "4711" "x1.5")))))
                :type 'user-error))

(ert-deftest az-org-clocking-formatter-writes-grouped-clocktable ()
  (with-temp-buffer
    (insert "* ERP\n"
            ":PROPERTIES:\n:COST_CENTER: 4711\n:END:\n"
            "** Review\n"
            "CLOCK: [2026-10-13 Tue 09:00]--[2026-10-13 Tue 10:00] =>  1:00\n"
            "* Mail\n"
            "CLOCK: [2026-10-13 Tue 11:00]--[2026-10-13 Tue 11:30] =>  0:30\n"
            "* Night\n"
            ":PROPERTIES:\n:COST_CENTER: After hours\n:CLOCK_FACTOR: 1.5\n:END:\n"
            "CLOCK: [2026-10-13 Tue 23:00]--[2026-10-13 Tue 23:40] =>  0:40\n"
            "#+BEGIN: clocktable :scope file :maxlevel 2 "
            ":properties (\"COST_CENTER\" \"CLOCK_FACTOR\") "
            ":inherit-props t :formatter az-org-clocktable-by-cost-center\n"
            "#+END:\n")
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (search-forward "#+BEGIN:")
    (beginning-of-line)
    (org-update-dblock)
    (let ((output (buffer-string)))
      (should (string-match-p "| Headline +| Factored +| Time" output))
      (should (string-match-p "| \\*Total time\\* +| \\*2.50h\\* +| \\*2.17h\\*" output))
      (should (string-match-p "| 4711 +| 1.00h +| 1.00h" output))
      (should (string-match-p "| After hours +| 1.00h +| 0.67h" output))
      (should (string-match-p "| No cost center +| 0.50h +| 0.50h" output))
      (should-not (string-match-p "COST_CENTER |" output)))))

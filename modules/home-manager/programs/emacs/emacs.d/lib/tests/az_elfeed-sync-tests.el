;; -*- lexical-binding: t; -*-
;; Run with:
;;   ELFEED=$(ls -d /nix/store/*-emacs-elfeed-2026*/share/emacs/site-lisp/elpa/elfeed-* | head -1)
;;   emacs --quick --batch --directory "$ELFEED" \
;;     --load tests/az_elfeed-sync-tests.el --funcall ert-run-tests-batch-and-exit
;; The network, the request queue and the elfeed commands are replaced by stubs.
(require 'ert)
(require 'cl-lib)
(require 'elfeed)

(load (expand-file-name "../az_elfeed.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest az-elfeed-sync-stops-when-server-unreachable ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'az-elfeed--server-reachable-p) (lambda () nil))
              ((symbol-function 'elfeed-protocol-fever-sync-pending-ids)
               (lambda (_host-url) (push 'pending calls)))
              ((symbol-function 'elfeed-update) (lambda () (push 'update calls))))
      (should-error (az-elfeed-sync) :type 'user-error)
      (should-not calls))))

(ert-deftest az-elfeed-sync-updates-after-pending-changes-are-sent ()
  (let ((calls nil)
        (queue-sizes '(0 2 1 0)))
    (cl-letf (((symbol-function 'az-elfeed--server-reachable-p) (lambda () t))
              ((symbol-function 'elfeed-queue-count-total) (lambda () (pop queue-sizes)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest arguments)
                 (push 'wait calls)
                 (apply function arguments)))
              ((symbol-function 'elfeed-protocol-fever-sync-pending-ids)
               (lambda (host-url)
                 (should (equal host-url az-elfeed-fever-host-url))
                 (push 'pending calls)))
              ((symbol-function 'elfeed-update) (lambda () (push 'update calls))))
      (az-elfeed-sync)
      (should (equal (reverse calls) '(pending wait wait update))))))

(ert-deftest az-elfeed-sync-refuses-while-requests-are-running ()
  (cl-letf (((symbol-function 'az-elfeed--server-reachable-p) (lambda () t))
            ((symbol-function 'elfeed-queue-count-total) (lambda () 3)))
    (should-error (az-elfeed-sync) :type 'user-error)))

(ert-deftest az-elfeed-mark-unread-uses-the-buffer-command ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'elfeed-search-tag)
               (lambda (&rest tags) (push (cons 'search tags) calls)))
              ((symbol-function 'elfeed-show-tag)
               (lambda (&rest tags) (push (cons 'show tags) calls))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (az-elfeed-mark-unread))
      (with-temp-buffer
        (setq major-mode 'elfeed-show-mode)
        (az-elfeed-mark-unread))
      (should (equal (reverse calls) '((search unread) (show unread)))))))

(ert-deftest az-elfeed-mark-read-outside-elfeed-is-an-error ()
  (with-temp-buffer
    (should-error (az-elfeed-mark-read) :type 'user-error)))

(ert-deftest az-elfeed-toggle-star-in-entry-buffer ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'elfeed-show-tag)
               (lambda (&rest tags) (push (cons 'tag tags) calls)))
              ((symbol-function 'elfeed-show-untag)
               (lambda (&rest tags) (push (cons 'untag tags) calls))))
      (with-temp-buffer
        (setq major-mode 'elfeed-show-mode)
        (setq-local elfeed-show-entry (elfeed-entry--create :tags '(star)))
        (az-elfeed-toggle-star)
        (setq-local elfeed-show-entry (elfeed-entry--create :tags '(unread)))
        (az-elfeed-toggle-star))
      (should (equal (reverse calls) '((untag star) (tag star)))))))
